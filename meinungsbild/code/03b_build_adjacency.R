## 03b_build_adjacency.R
## Build Kreis-level adjacency matrix from VG250_KRS shapefiles
## for BYM2 spatial smoothing in brms.
##
## Input:  german_election_data/data/shapefiles/2021/vg250_ebenen_0101/VG250_KRS.shp
## Output: meinungsbild/data/covariates/kreis_adjacency.rds  (binary adjacency matrix W)

library(sf)
library(spdep)
library(tidyverse)

gerda_root <- here::here()
mb_root    <- file.path(gerda_root, "meinungsbild")

# ---- 1. Load Kreis shapefile -------------------------------------------------

krs_sf <- st_read(file.path(
  gerda_root, "data", "shapefiles", "2021", "vg250_ebenen_0101", "VG250_KRS.shp"
), quiet = TRUE)

message("Loaded ", nrow(krs_sf), " Kreis polygons")

# AGS = 5-digit county code
krs_sf <- krs_sf |>
  mutate(county_code = AGS) |>
  filter(!is.na(county_code), nchar(county_code) == 5)

message("After filtering to 5-digit AGS: ", nrow(krs_sf), " Kreise")

# ---- 2. Load poststrat frame to get target county codes ----------------------

poststrat <- readRDS(file.path(mb_root, "data", "poststrat", "poststrat_kreis.rds"))
target_codes <- sort(unique(poststrat$county_code))
message("Target county codes from poststrat: ", length(target_codes))

# Filter shapefile to target codes
krs_sf <- krs_sf |>
  filter(county_code %in% target_codes) |>
  arrange(county_code)

# Check for missing
missing <- setdiff(target_codes, krs_sf$county_code)
if (length(missing) > 0) {
  message("WARNING: ", length(missing), " poststrat codes not in shapefile: ",
          paste(head(missing, 10), collapse = ", "))
}

extra <- setdiff(krs_sf$county_code, target_codes)
if (length(extra) > 0) {
  message("WARNING: ", length(extra), " shapefile codes not in poststrat: ",
          paste(head(extra, 10), collapse = ", "))
}

message("Matched Kreise: ", nrow(krs_sf))

# ---- 3. Build adjacency via polygon contiguity (queen) -----------------------

# poly2nb builds neighbor list from polygon contiguity
nb <- poly2nb(krs_sf, queen = TRUE)

# Check for islands (no neighbors)
n_islands <- sum(card(nb) == 0)
if (n_islands > 0) {
  message("WARNING: ", n_islands, " island Kreise with no neighbors")
  # Connect islands to nearest neighbor by centroid distance
  coords <- st_coordinates(st_centroid(krs_sf))
  for (i in which(card(nb) == 0)) {
    dists <- as.numeric(st_distance(st_centroid(krs_sf[i, ]), st_centroid(krs_sf)))
    dists[i] <- Inf  # exclude self
    nearest <- which.min(dists)
    nb[[i]] <- as.integer(nearest)
    # Make symmetric: add i to nearest's neighbors
    nb[[nearest]] <- sort(unique(c(nb[[nearest]], as.integer(i))))
    message("  Connected island ", krs_sf$county_code[i], " to ",
            krs_sf$county_code[nearest])
  }
}

# Convert to binary adjacency matrix
W <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Name rows/cols with county codes
rownames(W) <- krs_sf$county_code
colnames(W) <- krs_sf$county_code

message("\nAdjacency matrix: ", nrow(W), " x ", ncol(W))
message("Non-zero entries: ", sum(W > 0))
message("Mean neighbors: ", round(mean(rowSums(W)), 1))
message("Min neighbors: ", min(rowSums(W)), " | Max: ", max(rowSums(W)))

# Verify symmetry
stopifnot(isSymmetric(W))

# ---- 4. Save -----------------------------------------------------------------

dir.create(file.path(mb_root, "data", "covariates"), showWarnings = FALSE, recursive = TRUE)
saveRDS(W, file.path(mb_root, "data", "covariates", "kreis_adjacency.rds"))
message("\nSaved adjacency matrix to data/covariates/kreis_adjacency.rds")
