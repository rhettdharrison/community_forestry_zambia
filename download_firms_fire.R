# download_firms_fire.R
#
# Reads NASA FIRMS VIIRS S-NPP fire archive from firms_data/ zip, spatially
# assigns each detection to CF Core or 1 km Buffer zone, and saves
# fire_cfmg_zones.csv for use in Remote Sensing.qmd.
#
# Requires: firms_data/DL_FIRE_SV-C2_747029.zip (or any SV-C2 zip in firms_data/)

library(sf)
library(tidyverse)

sf_use_s2(FALSE)

# ── 1. Locate and extract the FIRMS zip ──────────────────────────────────────
data_dir <- file.path(getwd(), "firms_data")
zips <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)
if (length(zips) == 0) stop("No zip file found in firms_data/")
zip_path <- zips[1]
message("Using: ", basename(zip_path))

# Find the CSV inside the zip
csv_name <- unzip(zip_path, list = TRUE)$Name
csv_name <- csv_name[grepl("\\.csv$", csv_name)][1]
message("CSV inside zip: ", csv_name, " (",
        round(unzip(zip_path, list = TRUE)$Length[
          unzip(zip_path, list = TRUE)$Name == csv_name] / 1e6, 0), " MB)")

# Extract to firms_data/ (skip if already extracted)
csv_path <- file.path(data_dir, csv_name)
if (!file.exists(csv_path) || file.size(csv_path) < 1e6) {
  message("Extracting (this may take a minute) ...")
  unzip(zip_path, files = csv_name, exdir = data_dir, overwrite = TRUE)
} else {
  message("Already extracted: ", csv_name)
}

# ── 2. Read and filter fire detections ───────────────────────────────────────
message("Reading CSV ...")
fire_all <- read_csv(csv_path, show_col_types = FALSE) |>
  mutate(acq_date = as.Date(acq_date),
         Year     = year(acq_date)) |>
  filter(Year >= 2014, Year <= 2024)

message("Detections 2014–2024: ", nrow(fire_all))

# Note: extracted CSV kept in firms_data/ to avoid re-extracting on re-runs

# ── 3. Load CF polygons and buffer polygons ───────────────────────────────────
cf <- st_read(normalizePath("../CFMG_merged/cfmg_merged.shp"), quiet = TRUE) |>
  st_make_valid() |>
  mutate(NAME = str_to_sentence(NAME)) |>
  group_by(NAME, DATEEST, PROVINCE) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

buffers <- st_read(normalizePath("../CFMG_merged/cfmg_buffers_1km.shp"),
                   quiet = TRUE) |>
  st_make_valid() |>
  select(NAME, DATEEST)

# ── 4. Pre-filter to CF+buffer bounding box, then convert to sf ───────────────
# Get combined extent of all CF polygons + buffers (+ small margin)
bbox <- st_bbox(st_union(st_geometry(cf), st_geometry(buffers)))
margin <- 0.05   # ~5 km margin in degrees

fire_near <- fire_all |>
  filter(longitude >= bbox["xmin"] - margin,
         longitude <= bbox["xmax"] + margin,
         latitude  >= bbox["ymin"] - margin,
         latitude  <= bbox["ymax"] + margin)

message("Detections near CFs (after bbox filter): ", nrow(fire_near),
        " of ", nrow(fire_all))

message("Converting to spatial points ...")
fire_sf <- st_as_sf(fire_near,
                    coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)

# ── 5. Spatial join to Core and Buffer zones ──────────────────────────────────
message("Joining to CF cores ...")
fire_core <- st_join(fire_sf,
                     cf |> select(NAME, DATEEST, PROVINCE),
                     join = st_within, left = FALSE) |>
  mutate(zone = "Core") |>
  st_drop_geometry()

message("Joining to 1 km buffers ...")
fire_buffer <- st_join(fire_sf,
                       buffers |> select(NAME, DATEEST),
                       join = st_within, left = FALSE) |>
  mutate(zone     = "Buffer",
         PROVINCE = NA_character_) |>
  st_drop_geometry()

message("Core detections:   ", nrow(fire_core))
message("Buffer detections: ", nrow(fire_buffer))

# ── 6. Combine and save ───────────────────────────────────────────────────────
fire_zones <- bind_rows(fire_core, fire_buffer) |>
  mutate(zone = factor(zone, levels = c("Core", "Buffer"))) |>
  arrange(NAME, zone, acq_date)

out_csv <- file.path(getwd(), "fire_cfmg_zones.csv")
write_csv(fire_zones, out_csv)
message("Saved to ", out_csv)
message(nrow(fire_zones), " total  |  ",
        sum(fire_zones$zone == "Core"),   " core  |  ",
        sum(fire_zones$zone == "Buffer"), " buffer")
