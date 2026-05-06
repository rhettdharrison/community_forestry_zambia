# download_hansen_deforestation.R
#
# Downloads Hansen GFC 2024 lossyear tiles for Zambia and extracts annual
# tree cover loss (ha) for each CFMG polygon, 2014–2024.
#
# Output: ../data/cfm_hansen_loss_2014_2024.csv
# Run once; re-run only if shapefiles or tiles are updated.

library(terra)
library(sf)
library(exactextractr)
library(tidyverse)

# Extend download timeout — tiles are ~80–120 MB each
options(timeout = 3600)

# ── 1. Download Hansen GFC lossyear tiles ─────────────────────────────────────
# Four tiles cover the CF extent (8–16°S, 24–34°E)
tiles   <- c("00N_020E", "00N_030E", "10S_020E", "10S_030E")
version <- "GFC-2024-v1.12"
base_url <- paste0(
  "https://storage.googleapis.com/earthenginepartners-hansen/", version
)

# Check a downloaded tile is a readable GeoTIFF (catches corrupt/partial downloads)
is_valid_tif <- function(path) {
  if (!file.exists(path) || file.size(path) < 1e6) return(FALSE)
  tryCatch({ r <- rast(path); nrow(r) > 0 }, error = function(e) FALSE)
}

tile_dir <- file.path(getwd(), "hansen_tiles")
if (!dir.exists(tile_dir)) {
  ok <- dir.create(tile_dir, showWarnings = TRUE, recursive = TRUE)
  if (!ok) stop("Could not create directory: ", tile_dir)
}
message("Tile directory: ", tile_dir)

for (tile in tiles) {
  fname <- paste0("Hansen_", version, "_lossyear_", tile, ".tif")
  dest  <- file.path(tile_dir, fname)
  if (!is_valid_tif(dest)) {
    if (file.exists(dest))
      message("Invalid/incomplete file (", round(file.size(dest) / 1e6, 1),
              " MB) — re-downloading ", fname, " ...")
    else
      message("Downloading ", fname, " ...")
    result <- tryCatch(
      download.file(paste0(base_url, "/", fname), dest, mode = "wb"),
      error = function(e) { message("ERROR: ", e$message); 1L }
    )
    if (result != 0 || !is_valid_tif(dest)) {
      stop("Download failed for ", fname,
           "\nCheck URL: ", paste0(base_url, "/", fname),
           "\nFile size after attempt: ",
           if (file.exists(dest)) round(file.size(dest) / 1e6, 1) else 0, " MB")
    }
    message("  Saved: ", round(file.size(dest) / 1e6, 1), " MB")
  } else {
    message("Already downloaded and valid: ", fname,
            " (", round(file.size(dest) / 1e6, 1), " MB)")
  }
}

# ── 2. Build virtual mosaic ───────────────────────────────────────────────────
tile_files <- file.path(
  tile_dir, paste0("Hansen_", version, "_lossyear_", tiles, ".tif")
)

missing <- tile_files[!file.exists(tile_files)]
if (length(missing) > 0) {
  stop("Tile files missing after download:\n", paste(missing, collapse = "\n"))
}

vrt_path <- file.path(tile_dir, "lossyear_zambia.vrt")
lossyear_vrt <- tryCatch(
  vrt(tile_files, filename = vrt_path, overwrite = TRUE),
  error = function(e) {
    message("vrt() failed (", e$message, ") — falling back to terra::mosaic()")
    r_list <- lapply(tile_files, rast)
    mosaic(sprc(r_list))
  }
)
message("Mosaic ready. Extent: ", paste(as.vector(ext(lossyear_vrt)), collapse = ", "))

# ── 3. Load CF polygons and dissolve multi-polygon CFMGs ─────────────────────
sf_use_s2(FALSE)   # use GEOS instead of S2 — avoids strict validity checks
cf <- st_read(normalizePath("../CFMG_merged/cfmg_merged.shp"), quiet = TRUE) |>
  st_make_valid() |>
  mutate(NAME = str_to_sentence(NAME)) |>
  group_by(NAME, DATEEST) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_transform(32735) |>                                         # UTM 35S — metric CRS for Zambia
  mutate(area_ha = as.numeric(st_area(geometry)) / 10000) |>
  st_transform(4326)                                             # reproject back to WGS84 for extraction

message("Loaded ", nrow(cf), " CFMGs from shapefile")

# ── 4. Extract annual loss pixels per CF ──────────────────────────────────────
# Hansen lossyear pixel values: 0 = no loss, 1 = 2001, ..., 24 = 2024
# Pixel area ≈ 0.09 ha at Zambia latitudes (30m × 30m, slight latitude shrinkage)
pixel_ha <- 0.09

message("Extracting annual loss for ", nrow(cf), " CFMGs ...")

cf_loss <- exact_extract(
  lossyear_vrt,
  cf,
  function(df) {
    df |>
      filter(value > 0) |>
      mutate(year = as.integer(value) + 2000) |>
      filter(year >= 2014) |>
      group_by(NAME, year) |>
      summarise(loss_ha = sum(coverage_fraction) * pixel_ha, .groups = "drop")
  },
  summarize_df = TRUE,
  include_cols = "NAME"
)

# ── 5. Pivot wide and attach DATEEST and area ─────────────────────────────────
cf_attrs <- st_drop_geometry(cf)

cf_loss_wide <- cf_loss |>
  mutate(col = paste0("Y", year, "_loss_ha")) |>
  select(NAME, col, loss_ha) |>
  pivot_wider(names_from = col, values_from = loss_ha, values_fill = 0) |>
  left_join(cf_attrs, by = "NAME") |>
  relocate(NAME, DATEEST, area_ha)

out_csv <- file.path(getwd(), "cfm_hansen_loss_2014_2024.csv")
write_csv(cf_loss_wide, out_csv)
message("Done. Saved to ", out_csv)
message(nrow(cf_loss_wide), " CFMGs | years 2014–2024 | ",
        ncol(cf_loss_wide) - 3, " annual columns")
