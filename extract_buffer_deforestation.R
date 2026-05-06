# extract_buffer_deforestation.R
#
# Extracts annual Hansen GFC tree cover loss (ha) for each CFMG buffer zone,
# 2014–2024. Requires tiles already downloaded by download_hansen_deforestation.R.
#
# Output: cfm_hansen_buffer_loss_2014_2024.csv

library(terra)
library(sf)
library(exactextractr)
library(tidyverse)

sf_use_s2(FALSE)

# ── 1. Rebuild VRT from existing tiles ───────────────────────────────────────
tile_dir   <- file.path(getwd(), "hansen_tiles")
version    <- "GFC-2024-v1.12"
tiles      <- c("00N_020E", "00N_030E", "10S_020E", "10S_030E")
tile_files <- file.path(tile_dir, paste0("Hansen_", version, "_lossyear_", tiles, ".tif"))

missing <- tile_files[!file.exists(tile_files)]
if (length(missing) > 0)
  stop("Missing tiles — run download_hansen_deforestation.R first:\n",
       paste(missing, collapse = "\n"))

vrt_path     <- file.path(tile_dir, "lossyear_zambia.vrt")
lossyear_vrt <- vrt(tile_files, filename = vrt_path, overwrite = TRUE)
message("Mosaic ready")

# ── 2. Load buffer polygons ───────────────────────────────────────────────────
buffers <- st_read(normalizePath("../CFMG_merged/cfmg_buffers_1km.shp"),
                   quiet = TRUE) |>
  st_make_valid()

message("Loaded ", nrow(buffers), " buffer polygons")

# ── 3. Extract annual loss per buffer ─────────────────────────────────────────
pixel_ha <- 0.09

message("Extracting annual loss ...")
buf_loss <- exact_extract(
  lossyear_vrt,
  buffers,
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

# ── 4. Pivot wide and attach DATEEST and buffer area ─────────────────────────
buf_attrs <- st_drop_geometry(buffers) |> select(NAME, DATEEST, buffer_ha)

buf_loss_wide <- buf_loss |>
  mutate(col = paste0("Y", year, "_loss_ha")) |>
  select(NAME, col, loss_ha) |>
  pivot_wider(names_from = col, values_from = loss_ha, values_fill = 0) |>
  left_join(buf_attrs, by = "NAME") |>
  relocate(NAME, DATEEST, buffer_ha)

out_csv <- file.path(getwd(), "cfm_hansen_buffer_loss_2014_2024.csv")
write_csv(buf_loss_wide, out_csv)
message("Done. Saved to ", out_csv)
message(nrow(buf_loss_wide), " buffers | years 2014–2024 | ",
        ncol(buf_loss_wide) - 3, " annual columns")
