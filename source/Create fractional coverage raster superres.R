# ==============================================================================
# Rasterize Labeled Polygons to Create Fraction Rasters
# ------------------------------------------------------------------------------
# Purpose:
# This script converts a clean, labeled vector polygon file (shapefile) into a
# multi-layer raster. Each layer in the output raster represents the fractional
# cover of a specific label (e.g., 'inundated') within each pixel. The grid of
# the output raster is matched exactly to a corresponding Sentinel-2 image.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(terra)
library(sf)
library(dplyr)
library(googledrive)


# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Load Custom Utility Functions ---
source("source/spatial_processing_utils.R")
source("source/gdrive_utils.R")

# --- Primary Analysis Parameters ---
# study_site_name <- "Webbekomsbroek"
# study_site_name <- "Kloosterbeemden"
 study_site_name <- "Webbekomsbroek2"
# study_site_name <- "Schulensmeer"

# target_year <- 2020 
# target_year <- 2021
# target_year <- 2023
 target_year <- 2024

spatial_resolution <- "superres"

# --- Site-Specific Mappings 🔖 ---
site_abbreviations <- list(
  "Webbekomsbroek" = "WB",
  "Schulensmeer" = "SM",
  "Kloosterbeemden" = "KB",
  "Webbekomsbroek2" = "WB"
)

# --- Google Drive IDs ---
gdrive_ids <- list(
  labeled_shapefiles_folder = "1ylQ5_tsA2LPzAoa7BOg5-EydADjS7GL"
)

# --- Local Directory Paths ---
# UPDATED: Reverted to the relative path for portability.
data_root_dir <- "data"
output_root_dir <- "output"

# Cache for downloaded shapefiles.
local_paths <- list(
  labeled_shapefiles_cache = file.path(data_root_dir, "final_labels")
)

# --- Input File Paths (Constructed) ---
# This will now correctly resolve to "data/Sen2_superres/..."
s2_template_raster_path <- file.path(
  data_root_dir, "Sen2_superres",
  paste0(study_site_name, "_Sen2_", target_year, "_", spatial_resolution, ".tif")
)

# --- Output File Path (Constructed) ---
fractions_raster_output_path <- file.path(
  output_root_dir, "fraction_rasters", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_fractions_", spatial_resolution, ".tif")
)


# ==============================================================================
# 1️⃣ Authenticate with Google Drive
# ==============================================================================
message("Authenticating with Google Drive...")
drive_auth()
message("Authentication successful.")


# ==============================================================================
# 2️⃣ Acquire and Load Input Data
# ==============================================================================
message("\n--- Acquiring and loading input data for '", study_site_name, "' (", target_year, ") ---")

# --- 2.1 Download all labeled polygon shapefiles ------------------------------
message("Downloading all labeled polygon shapefiles...")
force_download_gdrive_folder(
  gdrive_folder_id = gdrive_ids$labeled_shapefiles_folder,
  local_path = local_paths$labeled_shapefiles_cache
)

# --- 2.2 Load the specific shapefile for the study site -----------------------
study_site_abbreviation <- site_abbreviations[[study_site_name]]

if (study_site_name == "Webbekomsbroek2") {
  labeled_shp_filename <- paste0("Labels_WB_", target_year, "_2.shp")
  message("NOTE: Using special filename convention for 'Webbekomsbroek2'.")
} else {
  labeled_shp_filename <- paste0("Labels_", study_site_abbreviation, "_", target_year, ".shp")
}

labeled_shp_path <- file.path(local_paths$labeled_shapefiles_cache, labeled_shp_filename)

if (!file.exists(labeled_shp_path)) {
  stop("FATAL: Labeled shapefile not found at: \n", labeled_shp_path)
}
labeled_polygons <- st_read(labeled_shp_path, quiet = TRUE)
message("Labeled polygons loaded successfully from: ", labeled_shp_filename)
if (!"Label" %in% names(labeled_polygons)) stop("FATAL: Shapefile must contain a 'Label' column.")

# --- 2.3 Standardize labels to correct inconsistencies ------------------------
message("Standardizing labels in the 'Label' column...")

if ("Reeds" %in% unique(labeled_polygons$Label)) {
  message("Found inconsistent label 'Reeds'. Standardizing to 'Reed'...")
  labeled_polygons <- labeled_polygons %>%
    mutate(Label = if_else(Label == "Reeds", "Reed", Label))
  message("Standardization complete.")
} else {
  message("No 'Reeds' label found; no changes needed.")
}

message("Final unique labels in the dataset:")
print(sort(unique(labeled_polygons$Label)))

# --- 2.4 Load the Sentinel-2 raster to use as a template ----------------------
message("Looking for template raster at: ", s2_template_raster_path)
if (!file.exists(s2_template_raster_path)) {
  stop("FATAL: Sentinel-2 template raster not found.")
}
s2_template_raster <- rast(s2_template_raster_path)
message("Sentinel-2 template raster loaded successfully.")


# ==============================================================================
# 3️⃣ Create the Fractional Cover Raster
# ==============================================================================
message("\n--- Creating the fractional cover raster ---")

if (st_crs(labeled_polygons) != st_crs(s2_template_raster)) {
  message("CRS mismatch detected. Reprojecting polygons to match raster template...")
  labeled_polygons <- st_transform(labeled_polygons, st_crs(s2_template_raster))
}

fractions_raster <- create_fraction_raster(labeled_polygons, s2_template_raster)

print(fractions_raster)


# ==============================================================================
# 4️⃣ Save the Output Raster
# ==============================================================================
message("\n--- Saving the output fractional raster ---")

output_dir <- dirname(fractions_raster_output_path)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

terra::writeRaster(
  fractions_raster,
  fractions_raster_output_path,
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)

message(paste("SUCCESS: Fractional raster saved to:\n", fractions_raster_output_path))

