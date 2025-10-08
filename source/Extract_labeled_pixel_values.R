# ==============================================================================
# Pixel-Level Analysis of Sentinel-2 and Land Cover Fractions
# ------------------------------------------------------------------------------
# Purpose:
# This script loads Sentinel-2 imagery and pre-calculated land cover fraction
# data. It aligns these datasets, calculates spectral indices, classifies
# pixels by land cover purity, and saves a final data table for analysis.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Load Custom Utility Functions ---
source("source/spatial_processing_utils.R")
source("source/gdrive_utils.R")

# --- Primary Analysis Parameters ---
# study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
 study_site_name <- "Webbekomsbroek2"

# Select the single target year for this analysis run.
# target_year <- 2020
# target_year <- 2021
# target_year <- 2023
 target_year <- 2024

# --- Root Directories ---
data_root_dir <- "data"
output_root_dir <- "output"

# --- Input File Paths ---
# Path to the fractional cover raster
fractions_raster_path <- file.path(
  output_root_dir, "fraction_rasters", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_fractions.tif")
)

# Path to the original Sentinel-2 raster
original_s2_raster_path <- file.path(
  data_root_dir, "Sen2",
  paste0(study_site_name, "_Sen2_", target_year, ".tif")
)

# --- Output File Paths ---
  # Path for the intermediate combined raster
  combined_raster_path <- file.path(
    output_root_dir, "combined_rasters", study_site_name, target_year,
    paste0(study_site_name, "_", target_year, "_S2_and_fractions_aligned.tif")
  )

# Path for the final CSV data table
final_csv_path <- file.path(
  output_root_dir, "pixel_data_tables", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_pixel_attributes.csv")
)

# Path for the final, all-inclusive raster with derived attributes
final_attributes_raster_path <- file.path( # <-- ADD THIS DEFINITION
  output_root_dir, "final_rasters", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_attributes_raster.tif")
)

# ==============================================================================
# 1️⃣ Load Input Data
# ==============================================================================
message("\n--- Loading input data for '", study_site_name, "' (", target_year, ") ---")

if (!file.exists(fractions_raster_path)) stop("FATAL: Fractions raster not found at: \n", fractions_raster_path)
fractions_raster <- rast(fractions_raster_path)
message("Fractions raster loaded successfully.")

if (!file.exists(original_s2_raster_path)) stop("FATAL: Sentinel-2 raster not found at: \n", original_s2_raster_path)
s2_raster_all_bands <- rast(original_s2_raster_path)
message("Sentinel-2 raster loaded successfully.")


# ==============================================================================
# 2️⃣ Align, Combine, and Save Rasters
# ==============================================================================
message("\n--- Aligning and combining rasters ---")

# --- 2.1 Filter Sentinel-2 to spectral bands ----------------------------------
s2_band_names <- names(s2_raster_all_bands)[grep("^B(\\d{1,2}A?)$", names(s2_raster_all_bands))]
if (length(s2_band_names) == 0) {
  warning("No spectral bands (starting with 'B') found. Using all bands.")
  s2_raster_filtered <- s2_raster_all_bands
} else {
  s2_raster_filtered <- subset(s2_raster_all_bands, s2_band_names)
  message(paste("Filtered Sentinel-2 to", length(s2_band_names), "spectral bands."))
}

# --- 2.2 Align S2 grid to fractions grid --------------------------------------
# This step ensures both rasters have the exact same geometry.
message("Aligning Sentinel-2 grid to match fractions raster...")
s2_raster_aligned <- terra::resample(s2_raster_filtered, fractions_raster, method = "near")
s2_raster_aligned <- terra::mask(s2_raster_aligned, fractions_raster[[1]]) # Ensure NA patterns match
message("Alignment complete.")

# --- 2.3 Combine -----------------------------------
message("Combining aligned rasters into a single object...")
combined_raster <- c(fractions_raster, s2_raster_aligned)
print(combined_raster)

# ==============================================================================
# 3️⃣ Extract Pixel Data to Data Frame
# ==============================================================================
message("\n--- Extracting pixel data to data frame ---")

# terra::values() is an efficient way to extract all pixel data
combined_pixel_data_df <- as.data.frame(values(combined_raster, na.rm = TRUE))
coords <- xyFromCell(combined_raster, which(!is.na(values(combined_raster[[1]]))))
combined_pixel_data_df <- cbind(as.data.frame(coords), combined_pixel_data_df)
colnames(coords) <- c("x_coord", "y_coord") 
combined_pixel_data_df <- cbind(as.data.frame(coords), combined_pixel_data_df)

message("Data frame created with ", nrow(combined_pixel_data_df), " pixels and ", ncol(combined_pixel_data_df), " columns.")


# ==============================================================================
# 4️⃣ Calculate Derived Attributes
# ==============================================================================
message("\n--- Calculating derived pixel attributes ---")

# --- 4.1 Scale Sentinel-2 bands to reflectance --------------------------------
s2_raw_band_cols <- names(combined_pixel_data_df)[grep("^B(\\d{1,2}A?)$", names(combined_pixel_data_df))]
for (col in s2_raw_band_cols) {
  combined_pixel_data_df[[paste0(col, "_scaled")]] <- combined_pixel_data_df[[col]] / 10000.0
}
message("Sentinel-2 bands scaled to reflectance.")

# --- 4.2 Calculate spectral indices -------------------------------------------
combined_pixel_data_df <- calculate_spectral_indices(combined_pixel_data_df)
message("Spectral indices calculated.")

# --- 4.3 Assign dominant label and mixture category ---------------------------
fraction_colnames <- names(fractions_raster)
combined_pixel_data_df <- classify_pixel_mixture(combined_pixel_data_df, fraction_colnames)
message("Dominant label and mixture category assigned.")

message("Forcing specific order on categorical columns...")

combined_pixel_data_df$dominant_label <- factor(
  combined_pixel_data_df$dominant_label, 
  levels = c("Inundated", "Not inundated", "Other", "Reed", "Uncertain")
)

# ==============================================================================
# 5️⃣ Integrate Derived Attributes into Final Raster
# ==============================================================================
message("\n--- Integrating derived attributes back into a final raster ---")

# --- 5.1 Identify all columns to be added as new raster layers ---------------

# Get the names of the newly created scaled reflectance bands
scaled_band_cols <- names(combined_pixel_data_df)[grep("_scaled$", names(combined_pixel_data_df))]

# Define the other derived attributes you want to add
other_attribute_cols <- c(
  "ndvi", "ndwi_mf", "mndwi11", "mndwi12", "ndmi_gao11", "str1", "str2",
  "dominant_label", "mixture_category"
)

# Combine all new attributes into a single list
attributes_to_add <- c(scaled_band_cols, other_attribute_cols)

message(
  "The following ", length(attributes_to_add),
  " attributes will be added as new raster layers:"
)
print(attributes_to_add)

# --- 5.2 Rasterize the attributes and combine with the original raster -------

# Use the utility function to create the new raster layers
derived_attribute_raster <- rasterize_attributes(
  df = combined_pixel_data_df,
  columns_to_rasterize = attributes_to_add,
  template_raster = combined_raster
)

# Add the new attribute layers to the existing combined raster
final_raster_with_attributes <- c(combined_raster, derived_attribute_raster)

message("Final raster with all attributes created successfully.")
print(final_raster_with_attributes)


# ==============================================================================
# 6️⃣ Save Final Outputs
# ==============================================================================
message("\n--- Saving final outputs (CSV and Raster) ---")

# --- 6.1 Save the final data table to CSV -------------------------------------
dir.create(dirname(final_csv_path), recursive = TRUE, showWarnings = FALSE)
write.csv(combined_pixel_data_df, file = final_csv_path, row.names = FALSE)
message("SUCCESS: Final data frame saved to:\n", final_csv_path)

# --- 6.2 Save the final, all-inclusive raster to GeoTIFF ----------------------
dir.create(dirname(final_attributes_raster_path), recursive = TRUE, showWarnings = FALSE)
writeRaster(
  final_raster_with_attributes,
  final_attributes_raster_path,
  overwrite = TRUE,
  gdal=c("COMPRESS=LZW")
)
message("SUCCESS: Final raster with all attributes saved to:\n", final_attributes_raster_path)

