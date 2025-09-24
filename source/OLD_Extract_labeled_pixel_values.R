# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#        Pixel-Level Analysis of Sentinel-2 Data and Land Cover Fractions
#
# This script loads pre-calculated land cover fraction data and original
# Sentinel-2 imagery. It aligns these datasets, extracts pixel values,
# calculates various spectral indices, classifies pixels based on label
# purity, and generates analytical outputs (data tables, plots).
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Part 1: Setup and Configuration
# ==============================================================================
message("Part 1: Initializing Setup and Configuration...")

# --- 1.1: Load Required Packages ---
message("--> Loading required R packages: terra, dplyr, ggplot2, tidyr")
if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required. Please install it.")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required. Please install it.")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required. Please install it.")
if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required. Please install it.")
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)
message("--> Packages loaded successfully. ✅")

# --- 1.2: Source Utility and Configuration Files ---
message("--> Sourcing utility and configuration files...")
# Define paths to helper scripts.
gdrive_utils_path <- "source/gdrive_utils.R"
spatial_utils_path <- "source/spatial_processing_utils.R"
config_path <- "source/config.R"

# Helper function to source files safely.
source_if_exists <- function(file_path, file_description) {
  if (!file.exists(file_path)) {
    stop(paste(file_description, "not found at:", normalizePath(file_path, mustWork = FALSE),
               ". Please check the path and ensure the file exists relative to your working directory: ", getwd()))
  }
  message("    Sourcing ", file_description, " from: ", normalizePath(file_path))
  source(file_path, local = FALSE, chdir = TRUE)
}

# Source the files. This loads project-specific variables from config.R.
source_if_exists(gdrive_utils_path, "Google Drive utilities (gdrive_utils.R)")
source_if_exists(config_path, "Configuration settings (config.R)")
source_if_exists(spatial_utils_path, "Spatial processing utilities (spatial_processing_utils.R)")

# --- 1.3: Define File Paths from Configuration ---
message("--> Defining input and output file paths...")
# Path to the pre-calculated fractional land cover raster.
fractions_raster_filename <- paste0(study_site_name, "_", study_year, "_fractions.tif")
fractions_raster_path <- file.path(output_root_dir,
                                   "fraction rasters", # Note: a typo might be intended here.
                                   study_site_name,
                                   study_year,
                                   fractions_raster_filename)

# Path to the original, full Sentinel-2 scene.
original_s2_data_subfolder <- "Sen2"
original_s2_filename <- paste0("Sen2_", study_site_name, "_", study_year, ".tif")
original_s2_raster_path <- file.path(data_root_dir,
                                     original_s2_data_subfolder,
                                     study_site_name,
                                     study_year,
                                     original_s2_filename)
message("--> Setup and configuration complete. ✅")

# --- End of Part 1 ---


## Part 2: Prepare and Combine Input Rasters
# ==============================================================================
message("\nPart 2: Preparing and Combining Input Rasters...")

# --- 2.1: Load and Filter Input Rasters ---
message("--> Loading fractional cover raster: ", basename(fractions_raster_path))
if (!file.exists(fractions_raster_path)) stop("ERROR: Fractions raster file not found at the specified path.")
fractions_raster <- terra::rast(fractions_raster_path)
message("--> Loading original Sentinel-2 raster: ", basename(original_s2_raster_path))
if (!file.exists(original_s2_raster_path)) stop("ERROR: Original Sentinel-2 raster file not found at the specified path.")
original_s2_raster_all_bands <- terra::rast(original_s2_raster_path)

message("--> Filtering Sentinel-2 raster to keep only spectral bands (starting with 'B')...")
s2_all_band_names <- names(original_s2_raster_all_bands)
# Regex matches B, then 1 or 2 digits, optionally followed by A (e.g., B02, B8A, B11).
bands_to_keep_indices <- grep("^B(\\d{1,2}A?)$", s2_all_band_names)

if (length(bands_to_keep_indices) == 0) {
  message("    [WARN] No bands starting with 'B' found. Using ALL available S2 bands.")
  s2_raster_filtered_bands <- original_s2_raster_all_bands
} else {
  selected_s2_band_names <- s2_all_band_names[bands_to_keep_indices]
  s2_raster_filtered_bands <- terra::subset(original_s2_raster_all_bands, selected_s2_band_names)
  message("    Kept S2 bands: ", paste(selected_s2_band_names, collapse=", "))
}

# --- 2.2: Spatially Align Sentinel-2 Data to Fractions Raster Grid ---
message("--> Spatially aligning Sentinel-2 data to the fractions raster grid...")
# To combine rasters, they must have identical geometry (CRS, extent, resolution).
# We use the fractions_raster as the geometric template and align the S2 data to it.
# 'resample' with 'method="near"' is a robust way to align grids without interpolating/altering pixel values.
s2_raster_aligned <- terra::resample(s2_raster_filtered_bands, fractions_raster, method = "near")

# Mask the aligned S2 data using the NA pattern from the fractions_raster.
# This ensures pixels that were not fully covered by labels are NA in all layers.
s2_raster_aligned <- terra::mask(s2_raster_aligned, fractions_raster[[1]])
message("--> Sentinel-2 data aligned successfully. ✅")

# --- 2.3: Combine and Save the Aligned Rasters ---
message("--> Combining fraction layers and aligned S2 bands into a single raster object...")
combined_raster_object <- c(fractions_raster, s2_raster_aligned)
message("    Combined raster created with ", nlyr(combined_raster_object), " layers:")
print(combined_raster_object)

# Define path for saving the combined raster.
combined_raster_output_dir <- file.path(output_root_dir, "combined_rasters", study_site_name, study_year)
if (!dir.exists(combined_raster_output_dir)) dir.create(combined_raster_output_dir, recursive = TRUE)
combined_raster_filename <- paste0(study_site_name, "_", study_year, "_S2fractions_pixel_layers_combined.tif")
combined_raster_file_path <- file.path(combined_raster_output_dir, combined_raster_filename)

message("--> Saving combined raster to: ", combined_raster_file_path)
tryCatch({
  terra::writeRaster(combined_raster_object, combined_raster_file_path, overwrite = TRUE,
                     gdal=c("COMPRESS=LZW", "BIGTIFF=YES"))
  message("--> Combined raster saved successfully. ✅")
}, error = function(e) {
  stop(paste("ERROR: Could not save combined raster. Reason:", e$message,
             "\nPlease check write permissions and available disk space."))
})
# --- End of Part 2 ---


## Part 3: Extract Pixel Data to Data Frame
# ==============================================================================
message("\nPart 3: Extracting Pixel Data to a Data Frame...")

# --- 3.1: Load Combined Raster and Extract Values ---
message("--> Loading the saved combined raster from file...")
if (!file.exists(combined_raster_file_path)) stop("ERROR: Saved combined raster file not found.")
loaded_combined_raster <- terra::rast(combined_raster_file_path)

message("--> Identifying valid pixels (where fraction data exists)...")
first_fraction_layer_name <- names(fractions_raster)[1]
first_layer_values_vec <- terra::values(loaded_combined_raster[[first_fraction_layer_name]], mat = FALSE)
valid_cells <- which(!is.na(first_layer_values_vec))

if (length(valid_cells) == 0) stop("ERROR: No valid pixels found in the combined raster.")
message("    Found ", format(length(valid_cells), big.mark=","), " valid pixels for data extraction.")

message("--> Extracting values for all layers at valid pixel locations...")
# Extracting with cell numbers is efficient. ID=FALSE returns a matrix of just the values.
all_values_matrix <- terra::extract(loaded_combined_raster, valid_cells)
coordinates_matrix <- terra::xyFromCell(loaded_combined_raster, valid_cells)
colnames(coordinates_matrix) <- c("x_coord", "y_coord")

message("--> Assembling the final pixel data frame...")
combined_pixel_data_df <- cbind(
  as.data.frame(coordinates_matrix),
  as.data.frame(all_values_matrix)
)
message("--> Pixel data frame created successfully. ✅")
message("    Dimensions: ", nrow(combined_pixel_data_df), " rows (pixels) by ", ncol(combined_pixel_data_df), " columns (attributes).")
# --- End of Part 3 ---


## Part 4: Calculate Derived Pixel Attributes
# ==============================================================================
message("\nPart 4: Calculating Derived Attributes (Scaled Bands, Indices, Labels)...")

# --- 4.1: Scale Sentinel-2 Bands ---
message("--> Scaling raw Sentinel-2 band values to reflectance (dividing by 10,000)...")
# Get raw S2 bands (those starting with "B" but not ending with "_scaled")
s2_raw_band_colnames <- names(combined_pixel_data_df)[grep("^B(\\d{1,2}A?)$", names(combined_pixel_data_df))]
if (length(s2_raw_band_colnames) > 0) {
  for (raw_band_col in s2_raw_band_colnames) {
    scaled_col_name <- paste0(raw_band_col, "_scaled")
    if (!scaled_col_name %in% names(combined_pixel_data_df)) { # Avoid re-scaling
      combined_pixel_data_df[[scaled_col_name]] <- combined_pixel_data_df[[raw_band_col]] / 10000.0
    }
  }
  message("    New columns with suffix '_scaled' added.")
} else {
  message("    [WARN] No raw S2 bands found to scale. Assuming scaled bands already exist if needed.")
}

# --- 4.2: Calculate Spectral Indices ---
message("--> Calculating spectral indices using scaled band values...")
# Define custom function for STR index, which expects scaled (0-1) input.
swir_to_str <- function(swir_scaled_values) {
  str_values <- ifelse(is.na(swir_scaled_values) | swir_scaled_values <= 0, NA_real_,
                       ((1 - swir_scaled_values)^2) / (2 * swir_scaled_values))
  return(str_values)
}

# Check for required scaled bands.
required_scaled_bands <- c("B03_scaled", "B04_scaled", "B8A_scaled", "B11_scaled", "B12_scaled")
if (!all(required_scaled_bands %in% names(combined_pixel_data_df))) {
  stop("ERROR: Not all required scaled bands (e.g., 'B03_scaled') found for index calculation.")
}

# Use dplyr::mutate to calculate all indices.
combined_pixel_data_df <- combined_pixel_data_df %>%
  mutate(
    ndvi = (.data[["B8A_scaled"]] - .data[["B04_scaled"]]) / (.data[["B8A_scaled"]] + .data[["B04_scaled"]]),
    ndwi_mf = (.data[["B03_scaled"]] - .data[["B8A_scaled"]]) / (.data[["B03_scaled"]] + .data[["B8A_scaled"]]),
    mndwi11 = (.data[["B03_scaled"]] - .data[["B11_scaled"]]) / (.data[["B03_scaled"]] + .data[["B11_scaled"]]),
    mndwi12 = (.data[["B03_scaled"]] - .data[["B12_scaled"]]) / (.data[["B03_scaled"]] + .data[["B12_scaled"]]),
    ndmi_gao11 = (.data[["B8A_scaled"]] - .data[["B11_scaled"]]) / (.data[["B8A_scaled"]] + .data[["B11_scaled"]]),
    str1 = swir_to_str(.data[["B11_scaled"]]),
    str2 = swir_to_str(.data[["B12_scaled"]])
  )
message("    Spectral indices (ndvi, ndwi_mf, mndwi*, ndmi*, str*) calculated. ✅")

# --- 4.3: Assign Dominant Label and Mixture Category ---
message("--> Assigning dominant label and mixture category based on fraction values...")
# Identify fraction columns by using the original layer names from fractions_raster.
fraction_colnames <- names(fractions_raster)
if (!all(fraction_colnames %in% names(combined_pixel_data_df))) stop("ERROR: Fraction columns not found in data frame.")
pixel_fractions_subset_df <- combined_pixel_data_df[, fraction_colnames, drop = FALSE]

# Define function to classify pixel based on max fraction.
determine_dominant_and_mixture <- function(fraction_row, label_names) {
  max_fraction <- max(fraction_row, na.rm = TRUE)
  if (!is.finite(max_fraction)) return(list(dominant_label = NA_character_, mixture_category = NA_character_))
  
  best_label_index <- which.max(fraction_row)
  dominant_label <- label_names[best_label_index]
  
  mixture_category <- if (max_fraction >= 0.9) { "pure" }
  else if (max_fraction > 0.6) { "mixed" }
  else { "very_mixed" }
  
  return(list(dominant_label = dominant_label, mixture_category = mixture_category))
}

# Apply the function to all pixels (rows).
labeling_results_list <- apply(pixel_fractions_subset_df, 1, determine_dominant_and_mixture, label_names = fraction_colnames)

# Add the new classification columns to the main data frame.
combined_pixel_data_df$dominant_label    <- sapply(labeling_results_list, `[[`, "dominant_label")
combined_pixel_data_df$mixture_category <- sapply(labeling_results_list, `[[`, "mixture_category")
message("    Columns 'dominant_label' and 'mixture_category' added. ✅")
# --- End of Part 4 ---


## Part 5: Generate and Save Outputs
# ==============================================================================
message("\nPart 5: Generating Final Outputs (CSV and Plots)...")

# --- 5.1: Save Final Data Frame to CSV ---
message("--> Saving the final augmented data frame to a CSV file...")
# Define output file path.
csv_output_dir <- file.path(output_root_dir, "pixel_data_tables", study_site_name, study_year)
if (!dir.exists(csv_output_dir)) dir.create(csv_output_dir, recursive = TRUE)
final_csv_filename <- paste0(study_site_name, "_", study_year, "_final_pixel_attributes.csv")
final_csv_filepath <- file.path(csv_output_dir, final_csv_filename)

tryCatch({
  write.csv(combined_pixel_data_df, file = final_csv_filepath, row.names = FALSE)
  message("--> Final data frame saved successfully to: ", final_csv_filepath, " ✅")
}, error = function(e) {
  message("[WARN] Could not save the data frame as a CSV file. Reason: ", e$message)
})


message("\n--- Script processing complete. ---")