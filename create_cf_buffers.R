# create_cf_buffers.R
#
# Creates 1 km buffer zones around each CFMG polygon.
# Areas where a buffer overlaps a neighbouring CF are excised.
#
# Output: ../CFMG_merged/cfmg_buffers_1km.shp

library(sf)
library(tidyverse)

sf_use_s2(FALSE)

# ── 1. Load and dissolve CF polygons ─────────────────────────────────────────
cf <- st_read(normalizePath("../CFMG_merged/cfmg_merged.shp"), quiet = TRUE) |>
  st_make_valid() |>
  mutate(NAME = str_to_sentence(NAME)) |>
  group_by(NAME, DATEEST) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

message("Loaded ", nrow(cf), " CFMGs")

# ── 2. Project to UTM 35S (EPSG:32735) for accurate metric distances ─────────
cf_proj <- st_transform(cf, 32735)

# ── 3. Create 1 km buffers ────────────────────────────────────────────────────
buffers_raw <- st_buffer(cf_proj, dist = 1000)

# ── 4. Excise all CF areas from every buffer ──────────────────────────────────
# st_union(cf_proj) gives a single geometry covering all CFs.
# st_difference subtracts it from each buffer row, removing:
#   - the CF's own interior (so buffers are rings, not discs)
#   - any neighbouring CF polygons that fall within the 1 km ring
all_cfs <- st_union(cf_proj)
buffers_clean <- st_difference(buffers_raw, all_cfs)

# ── 5. Add buffer area (ha) ───────────────────────────────────────────────────
buffers_clean <- buffers_clean |>
  mutate(buffer_ha = as.numeric(st_area(geometry)) / 10000)

message("Buffer areas (ha): min = ", round(min(buffers_clean$buffer_ha), 1),
        "  median = ", round(median(buffers_clean$buffer_ha), 1),
        "  max = ", round(max(buffers_clean$buffer_ha), 1))

# ── 6. Reproject to WGS84 and save ───────────────────────────────────────────
buffers_wgs84 <- st_transform(buffers_clean, 4326)

out_path <- normalizePath("../CFMG_merged/cfmg_buffers_1km.shp", mustWork = FALSE)
st_write(buffers_wgs84, out_path, delete_dsn = TRUE, quiet = TRUE)
message("Saved: ", out_path)

# Also save as a single-file GeoPackage for easy sharing
gpkg_path <- file.path(getwd(), "cfmg_buffers_1km.gpkg")
st_write(buffers_wgs84, gpkg_path, layer = "cfmg_buffers_1km",
         delete_dsn = TRUE, quiet = TRUE)
message("Saved (shareable): ", gpkg_path)
message(nrow(buffers_wgs84), " buffer polygons written")
