# 1. --- Setup ----
# Ensure these packages are installed: install.packages(c("terra"))
message("Loading required R packages...")
if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Package 'terra' is required but not installed. Please install it.")
}
library(terra)

message("Setup complete.")

# --- 2. Source Utility and Configuration Files ---
message("Sourcing utility and configuration files...")

gdrive_utils_path <- "source/gdrive_utils.R" 
spatial_utils_path <- "source/spatial_processing_utils.R" 
config_path <- "source/config.R"          

source_if_exists <- function(file_path, file_description) {
  if (!file.exists(file_path)) {
    stop(paste(file_description, "not found at:", normalizePath(file_path, mustWork = FALSE),
               ". Please check the path and ensure the file exists relative to your working directory: ", getwd()))
  }
  message("Sourcing ", file_description, " from: ", normalizePath(file_path))
  # Source into the global environment (or the calling environment) so functions are directly available
  source(file_path, local = FALSE, chdir = TRUE) 
}

# Source utility and config files
source_if_exists(gdrive_utils_path, "Google Drive utilities (gdrive_utils.R)")
source_if_exists(config_path, "Configuration settings (config.R)") # Loads variables like study_site_name
source_if_exists(spatial_utils_path, "Spatial processing utilities (spatial_processing_utils.R)")

# Path to the saved multi-layer fractional coverage raster
fractions_raster_filename <- paste0(study_site_name, "_", study_year, "_fractions.tif")
fractions_raster_path <- file.path(output_root_dir, 
                                   "fraction rastes", # Note: check if this should be "fraction rasters"
                                   study_site_name, 
                                   study_year, 
                                   fractions_raster_filename)
message(paste("Expected path for fractions raster:", fractions_raster_path))

# --- Path to the ORIGINAL, FULL Sentinel-2 Raster ---
original_s2_data_subfolder <- "Sen2" # Example: as in your initial script
original_s2_filename <- paste0("Sen2_", study_site_name, "_", study_year, ".tif") # Example: as in your initial script

original_s2_raster_path <- file.path(data_root_dir,
                                     original_s2_data_subfolder,
                                     study_site_name,
                                     study_year,
                                     original_s2_filename)
message(paste("Expected path for ORIGINAL full S2 raster:", original_s2_raster_path))

# CSV output parameters (optional, for saving the final data frame)
save_combined_df <- TRUE # Set to FALSE if you don't want to save the CSV
output_df_filename <- paste0(study_site_name, "_", study_year, "_pixel_data_S2_and_fractions.csv")
output_csv_directory <- dirname(fractions_raster_path)
output_df_full_path <- file.path(output_csv_directory, output_df_filename)

message("Configuration loaded.")

# 3. --- Load Input Rasters ---
message("Loading input rasters...")

if (!file.exists(fractions_raster_path)) {
  stop(paste("ERROR: Fractions raster file not found at:", fractions_raster_path,
             "\nPlease check the path and ensure the file was saved correctly from the previous script."))
}
fractions_raster <- terra::rast(fractions_raster_path)
message("Fractions raster loaded successfully.")
print(fractions_raster)

if (!file.exists(original_s2_raster_path)) {
  stop(paste("ERROR: Original full Sentinel-2 raster file not found at:", original_s2_raster_path,
             "\nPlease provide the correct path to the complete S2 scene."))
}
original_s2_raster_all_bands <- terra::rast(original_s2_raster_path)
message("Original full Sentinel-2 raster loaded successfully.")
print(original_s2_raster)

message("Input rasters loaded.")

# --- Filter Sentinel-2 bands to keep only those starting with "B" ---
message("INFO: Filtering Sentinel-2 raster to keep only bands starting with 'B'.")
s2_all_band_names <- names(original_s2_raster_all_bands)
bands_to_keep_indices <- startsWith(s2_all_band_names, "B")

if (sum(bands_to_keep_indices) == 0) {
  message("WARNING: No bands starting with 'B' found in the original Sentinel-2 raster. ",
          "Proceeding with ALL bands for S2 data extraction, or check raster band names if 'B' bands are expected.")
  # Fallback: use the raster with all bands if no "B" bands are found.
  # If "B" bands are absolutely essential, you might want to 'stop()' here instead.
  original_s2_raster <- original_s2_raster_all_bands 
} else {
  selected_s2_band_names <- s2_all_band_names[bands_to_keep_indices]
  original_s2_raster <- terra::subset(original_s2_raster_all_bands, selected_s2_band_names)
  message(paste("INFO: Kept the following Sentinel-2 bands:", paste(selected_s2_band_names, collapse=", ")))
}
message("INFO: Dimensions of S2 raster to be used for extraction (potentially band-filtered):")
print(original_s2_raster) # This is now the (potentially filtered) S2 raster we'll use

message("Input rasters (S2 potentially band-filtered) loaded.")

# 4. --- Extract S2 and Fraction Values for Valid Pixels ---
message("INFO: Starting extraction of fraction values and original Sentinel-2 values.")

# 4.1. Verify geometric compatibility for cell-based extraction
message("INFO: Verifying geometric compatibility (CRS, Resolution, Origin) between fractions raster and original S2 raster...")
compatible_crs <- compareGeom(fractions_raster, original_s2_raster, crs = TRUE, res = FALSE, ext = FALSE, rowcol = FALSE, stopOnError = FALSE)
compatible_res <- all(terra::res(fractions_raster) == terra::res(original_s2_raster))
# Origin check can be tricky due to floating point; compare with a tolerance
origin_diff <- abs(terra::origin(fractions_raster) - terra::origin(original_s2_raster))
compatible_origin <- all(origin_diff < (min(terra::res(fractions_raster)) * 1e-3)) # Tolerance relative to pixel size

if (compatible_crs && compatible_res && compatible_origin) {
  message("INFO: CRS, resolution, and origin are compatible. Cell-based extraction should be reliable.")
} else {
  message(paste("WARNING: CRS, resolution, or origin might not be perfectly compatible for direct cell number transfer.",
                "CRS match:", compatible_crs,
                "Resolution match:", compatible_res,
                "Origin match (approx):", compatible_origin,
                "If issues arise, consider extracting S2 data by coordinates instead of cell numbers."))
  # Forcing stop if not compatible, as cell numbers would be meaningless otherwise.
  # You could implement coordinate-based extraction as a fallback here.
  if (!compatible_crs || !compatible_res || !compatible_origin) {
    stop("FATAL ERROR: CRS, resolution, or origin are not compatible enough for reliable cell-based extraction between the rasters.")
  }
}

# 4.2. Identify valid cell numbers from the fractions_raster
message("INFO: Identifying valid (non-NA) pixels from the fractions raster...")
first_layer_fractions <- fractions_raster[[1]]
first_layer_values_vec <- terra::values(first_layer_fractions, mat = FALSE)
valid_cells <- which(!is.na(first_layer_values_vec))

if (length(valid_cells) == 0) {
  stop("ERROR: No valid (non-NA) pixels found in the loaded fractions raster. Cannot extract data.")
}
message(paste("INFO: Found", length(valid_cells), "valid pixels for data extraction."))

# 4.3. Extract fraction values for these valid cells
message("INFO: Extracting fraction values...")
fraction_values_matrix <- terra::extract(fractions_raster, valid_cells)

# 4.4. Extract original Sentinel-2 values from the FULL S2 raster for the same cell numbers
message("INFO: Extracting original Sentinel-2 values from the full S2 raster...")
# This relies on the cell numbers from fractions_raster being valid and corresponding
# to the same locations in original_s2_raster due to shared grid properties (CRS, res, origin).
s2_values_matrix <- terra::extract(original_s2_raster, valid_cells)

# Ensure S2 column names are descriptive
s2_original_band_names <- names(original_s2_raster)
if(is.null(s2_original_band_names) || length(s2_original_band_names) != ncol(s2_values_matrix) || any(s2_original_band_names == "")) {
  s2_col_names <- paste0("S2_Band", seq_len(ncol(s2_values_matrix)))
} else {
  s2_col_names <- paste0("S2_Original_", s2_original_band_names) # Added "Original" to distinguish
}
colnames(s2_values_matrix) <- s2_col_names

# 4.5. Get coordinates for these valid cells (from fractions_raster grid)
message("INFO: Extracting coordinates (x, y) for valid pixels...")
coordinates_matrix <- terra::xyFromCell(fractions_raster, valid_cells)
colnames(coordinates_matrix) <- c("x_coord", "y_coord")

# 4.6. Combine all extracted data into a single data frame
message("INFO: Combining extracted coordinates, S2 values, and fraction values...")
combined_pixel_data_df <- cbind(
  as.data.frame(coordinates_matrix),
  as.data.frame(s2_values_matrix),
  as.data.frame(fraction_values_matrix)
)

message("INFO: Data extraction and combination complete.")
message(paste("INFO: Resulting data frame has", nrow(combined_pixel_data_df), "rows (pixels) and", ncol(combined_pixel_data_df), "columns."))

message("INFO: Displaying the first few rows of the combined data frame:")
print(head(combined_pixel_data_df))

# 4.7. Optional: Save the combined data frame
if (save_combined_df) {
  if (!exists("output_csv_directory")) {
    if (exists("fractions_raster_path") && !is.null(fractions_raster_path)) {
      output_csv_directory <- dirname(fractions_raster_path)
    } else {
      output_csv_directory <- file.path(output_root_dir,
                                        "pixel_data_tables",
                                        study_site_name,
                                        study_year)
    }
  }
  if (!dir.exists(output_csv_directory)) {
    dir.create(output_csv_directory, recursive = TRUE)
    message(paste("INFO: Created directory for CSV output:", output_csv_directory))
  }
  output_df_full_path <- file.path(output_csv_directory, output_df_filename)
  
  tryCatch({
    write.csv(combined_pixel_data_df, output_df_full_path, row.names = FALSE)
    message(paste("INFO: Combined pixel data frame saved to:", output_df_full_path))
  }, error = function(e) {
    warning(paste("WARNING: Could not save combined data frame as CSV:", e$message))
  })
}
