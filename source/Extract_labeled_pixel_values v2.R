# 1. --- Setup ----
# Ensure these packages are installed: install.packages(c("terra"))
message("Loading required R packages...")
if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Package 'terra' is required but not installed. Please install it.")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required but not installed. Please install it.")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Package 'tidyr' is required but not installed. Please install it.")
}

library(terra)
library(ggplot2)
library(tidyr)

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
                                   "fraction rasters", # Note: check if this should be "fraction rasters"
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

# --- 3: Load Input Rasters & Filter S2 Bands  ---
message("Loading input rasters...")

if (!file.exists(fractions_raster_path)) {
  stop(paste("ERROR: Fractions raster file not found at:", fractions_raster_path))
}
fractions_raster <- terra::rast(fractions_raster_path)
message("Fractions raster loaded successfully.")
# print(fractions_raster) # Printing later with combined

if (!file.exists(original_s2_raster_path)) {
  stop(paste("ERROR: Original full Sentinel-2 raster file not found at:", original_s2_raster_path))
}
original_s2_raster_all_bands <- terra::rast(original_s2_raster_path)
message("Original full Sentinel-2 raster (all bands) loaded successfully.")

# Filter S2 bands
message("INFO: Filtering Sentinel-2 raster to keep only bands starting with 'B'.")
s2_all_band_names <- names(original_s2_raster_all_bands)
bands_to_keep_indices <- startsWith(s2_all_band_names, "B")

if (sum(bands_to_keep_indices) == 0) {
  message("WARNING: No bands starting with 'B' found. Using ALL S2 bands.")
  s2_raster_filtered_bands <- original_s2_raster_all_bands
} else {
  selected_s2_band_names <- s2_all_band_names[bands_to_keep_indices]
  s2_raster_filtered_bands <- terra::subset(original_s2_raster_all_bands, selected_s2_band_names)
  message(paste("INFO: Kept S2 bands:", paste(selected_s2_band_names, collapse=", ")))
}

# --- 3.5: Align Filtered S2 Raster to Fractions Raster Grid ---
message("INFO: Starting Section 3.5 - Aligning filtered S2 data to fractions_raster grid.")

# We need to ensure the filtered S2 data has the exact same extent, resolution,
# and NA pattern (for relevant areas) as fractions_raster.
# This involves cropping s2_raster_filtered_bands to the extent of fractions_raster,
# and then masking it by a layer from fractions_raster to align NAs.

# 1. Verify CRS and Resolution compatibility before crop/mask
# (Origin will be aligned by crop/resample if needed, but ideally already same)
crs_fractions <- terra::crs(fractions_raster)
crs_s2_filtered <- terra::crs(s2_raster_filtered_bands)
if (crs_fractions != crs_s2_filtered) {
  message(paste("WARNING: CRS mismatch between fractions_raster ('", crs_fractions,
                "') and filtered S2 raster ('", crs_s2_filtered,
                "'). Attempting to reproject S2 data. THIS MAY ALTER S2 VALUES."))
  # This reprojection is a fallback; ideally, they are already in the same S2 original CRS.
  # The fractions_raster was derived from S2 original CRS, so this should ideally not trigger.
  s2_raster_filtered_bands <- terra::project(s2_raster_filtered_bands, crs_fractions, method="near") # "near" to preserve S2 values as much as possible if reproj. needed
}

# 2. Crop the (potentially reprojected) S2 raster to the extent of fractions_raster.
# Then resample to ensure exact grid alignment if resolutions were slightly off,
# or if crop snapping isn't perfect. Using fractions_raster as the template for resample.
message("INFO: Cropping and resampling filtered S2 data to match fractions_raster geometry...")
# Using resample is more robust for achieving perfect alignment than just crop.
# It will crop to the extent of 'fractions_raster' and match its grid.
# Use method "near" (nearest neighbor) to avoid altering S2 values significantly during resampling.
s2_raster_aligned <- terra::resample(s2_raster_filtered_bands, fractions_raster, method = "near")

# 3. Mask the aligned S2 data using the NA pattern from the first layer of fractions_raster.
# This ensures that if a pixel is NA in fractions_raster (meaning it wasn't "fully covered"),
# it will also be NA in the S2 layers of the combined raster.
message("INFO: Masking aligned S2 data with fractions_raster's NA pattern...")
s2_raster_aligned <- terra::mask(s2_raster_aligned, fractions_raster[[1]])

message("INFO: Filtered S2 data aligned with fractions_raster.")

# --- 3.6: Combine Fractions and Aligned S2 into a Single SpatRaster and Save ---
message("INFO: Starting Section 3.6 - Combining and saving rasters.")

# Combine the fractions_raster and the now perfectly aligned s2_raster_aligned
# Ensure layer names are unique; terra handles this by appending numbers if not,
# but our S2 bands are prefixed "S2_Original_B" and fraction names are labels, so should be fine.
combined_raster_object <- c(fractions_raster, s2_raster_aligned)

message("INFO: Combined raster object created in memory with all desired layers:")
print(combined_raster_object) # Print details of the combined raster

# Define path for saving the combined raster
combined_raster_filename <- paste0(study_site_name, "_", study_year, "_S2fractions_pixel_layers_combined.tif")
if (exists("output_csv_directory") && !is.null(output_csv_directory)) { # Use CSV dir if defined from previous script context
  combined_raster_output_dir <- output_csv_directory
} else if (exists("fractions_raster_path") && !is.null(fractions_raster_path)) {
  combined_raster_output_dir <- dirname(fractions_raster_path)
} else {
  combined_raster_output_dir <- file.path(output_root_dir, "combined_rasters", study_site_name, study_year)
}
if (!dir.exists(combined_raster_output_dir)) {
  dir.create(combined_raster_output_dir, recursive = TRUE)
}
combined_raster_file_path <- file.path(combined_raster_output_dir, combined_raster_filename)

message(paste("INFO: Saving combined raster to:", combined_raster_file_path))
tryCatch({
  terra::writeRaster(combined_raster_object, combined_raster_file_path, overwrite = TRUE,
                     gdal=c("COMPRESS=LZW", "BIGTIFF=YES")) # Use BIGTIFF if it might exceed 4GB
  message("INFO: Combined raster saved successfully.")
}, error = function(e) {
  # Provide more context in the error if saving fails
  stop(paste("ERROR: Could not save combined raster to '", combined_raster_file_path, "'. Reason: ", e$message,
             "\nPlease check write permissions and available disk space.", sep=""))
})

# --- Section 4: Load Saved Combined Raster and Extract Values ---
message("INFO: Starting Section 4 - Load SAVED combined raster and extract values.")

# 4.1. Load the combined raster that was just saved
message(paste("INFO: Loading the saved combined raster from:", combined_raster_file_path))
if (!file.exists(combined_raster_file_path)) {
  stop(paste("ERROR: Saved combined raster file ('", combined_raster_file_path ,"') not found. Aborting data extraction.", sep=""))
}
loaded_combined_raster <- terra::rast(combined_raster_file_path)
message("INFO: Loaded combined raster details:")
print(loaded_combined_raster)

# 4.2. Identify valid cell numbers from the loaded_combined_raster
# Use the first layer (which was the first fraction layer) to find non-NA cells.
# These cells should have data across all layers if the alignment and masking in 3.5 worked.
message("INFO: Identifying valid (non-NA) pixels from the loaded combined raster...")
# Ensure we are using a layer that represents the mask of "fully covered pixels"
# This should be any of the fraction layers, as they were already masked.
first_fraction_layer_name <- names(fractions_raster)[1] # Get name of first fraction layer
if (!first_fraction_layer_name %in% names(loaded_combined_raster)) {
  stop(paste("Error: Expected first fraction layer '", first_fraction_layer_name, "' not found in loaded_combined_raster. Check layer names.", sep=""))
}
first_layer_combined_values_vec <- terra::values(loaded_combined_raster[[first_fraction_layer_name]], mat = FALSE)
valid_cells <- which(!is.na(first_layer_combined_values_vec))

if (length(valid_cells) == 0) {
  stop("ERROR: No valid (non-NA) pixels found in the loaded combined raster based on its first fraction layer. Cannot extract data.")
}
message(paste("INFO: Found", length(valid_cells), "valid pixels for data extraction from combined raster."))

# 4.3. Extract ALL layer values for these valid cells from the loaded_combined_raster
message("INFO: Extracting all layer values from loaded combined raster for valid cells...")
all_values_matrix <- terra::extract(loaded_combined_raster, valid_cells)
# Column names in all_values_matrix will be the layer names from loaded_combined_raster.

# 4.5. Get coordinates for these valid cells (from loaded_combined_raster grid)
message("INFO: Extracting coordinates (x, y) for valid pixels...")
coordinates_matrix <- terra::xyFromCell(loaded_combined_raster, valid_cells)
colnames(coordinates_matrix) <- c("x_coord", "y_coord")

# 4.6. Combine coordinates and extracted values into a single data frame
message("INFO: Combining extracted coordinates and all layer values...")
combined_pixel_data_df <- cbind(
  as.data.frame(coordinates_matrix),
  as.data.frame(all_values_matrix) # Contains S2 (filtered) and fraction values
)

message("INFO: Data extraction from saved combined raster and combination complete.")
message(paste("INFO: Resulting data frame has", nrow(combined_pixel_data_df), "rows (pixels) and", ncol(combined_pixel_data_df), "columns."))
message("INFO: Displaying the first few rows of the combined data frame:")
print(head(combined_pixel_data_df))


# --- Part 5: Assign Mixture Level and Base Label ---
message("INFO: Starting Part 5 - Assigning 'mixture_level' and 'base_label' to each pixel.")

# Check if combined_pixel_data_df exists
if (!exists("combined_pixel_data_df") || !is.data.frame(combined_pixel_data_df)) {
  stop("ERROR: 'combined_pixel_data_df' is not available. Cannot assign labels.")
}

# --- Part 5: Rescale Sen2 bands and calculate indices for inundation (based on Tytti's script) ---

# Identify Sentinel-2 band columns (e.g., "S2_Original_B02")
s2_band_colnames <- names(combined_pixel_data_df)[startsWith(names(combined_pixel_data_df), "B")]

for (raw_band_col in s2_band_colnames) {
  scaled_col_name <- paste0(raw_band_col, "_scaled")
  combined_pixel_data_df[[scaled_col_name]] <- combined_pixel_data_df[[raw_band_col]] / 10000.0
}
message("INFO: Scaled S2 band columns (e.g., B04_scaled) added to combined_pixel_data_df.")


swir_to_str <- function(swir_band_values) {
  # This function now expects ALREADY SCALED SWIR band values (e.g., 0-1 reflectance)
  # because the /10000 scaling is done before this function is called.
  
  # Original formula: ((1 - swir_band_values)^2) / (2 * swir_band_values)
  # Handle cases where swir_band_values (which are scaled) are <= 0 or NA
  str_values <- ifelse(is.na(swir_band_values) | swir_band_values <= 0, # Corrected variable name
                       NA_real_, 
                       ((1 - swir_band_values)^2) / (2 * swir_band_values))
  return(str_values)
}

# 3. Define required S2 band column names (as they appear in your data frame)
# Based on your last clarification ("Sen2 bands start with B"), these are likely "B03", "B04", etc.
required_bands_for_indices <- c("B03_scaled", "B04_scaled", "B8A_scaled", "B11_scaled", "B12_scaled")

# Check if all required band columns are present in combined_pixel_data_df
missing_bands <- required_bands_for_indices[!(required_bands_for_indices %in% names(combined_pixel_data_df))]

if (length(missing_bands) > 0) {
  stop(paste("Error: Required S2 band columns for indices are missing from 'combined_pixel_data_df':", 
             paste(missing_bands, collapse=", "),
             "\nAvailable columns in df:", paste(names(combined_pixel_data_df), collapse=", "),
             "\nPlease ensure S2 band columns are named correctly (e.g., 'B03', 'B04', 'B8A', 'B11', 'B12')."))
}
message(" -> All required S2 bands for indices are present in the data frame.")

# 4. Calculate Indices
if (nrow(combined_pixel_data_df) == 0) {
  message("WARN: 'combined_pixel_data_df' is empty. Adding empty index columns.")
  combined_pixel_data_df <- combined_pixel_data_df %>%
    dplyr::mutate(
      ndvi = numeric(0), 
      ndwi_mf = numeric(0),       # McFeeters NDWI
      mndwi11 = numeric(0),     # MNDWI using B11
      mndwi12 = numeric(0),     # MNDWI using B12
      ndmi_gao11 = numeric(0),  # NDMI (Gao) using B11
      str1 = numeric(0),        # STR using B11
      str2 = numeric(0)         # STR using B12
    )
} else {
  message(" -> Calculating spectral indices...")
  # Using .data[["COLUMN_NAME"]] for robustness within dplyr::mutate
  combined_pixel_data_df <- combined_pixel_data_df %>%
    dplyr::mutate(
      # NDVI = (NIR - Red) / (NIR + Red)  => (B8A - B04) / (B8A + B04)
      ndvi = (.data[["B8A_scaled"]] - .data[["B04_scaled"]]) / (.data[["B8A_scaled"]] + .data[["B04_scaled"]]),
      
      # NDWI (McFeeters, 1996) = (Green - NIR) / (Green + NIR) => (B03 - B8A) / (B03 + B8A)
      ndwi_mf = (.data[["B03_scaled"]] - .data[["B8A_scaled"]]) / (.data[["B03_scaled"]] + .data[["B8A_scaled"]]),
      
      # MNDWI (Xu, 2006 - using SWIR1/B11) = (Green - SWIR1) / (Green + SWIR1) => (B03 - B11) / (B03 + B11)
      mndwi11 = (.data[["B03_scaled"]] - .data[["B11_scaled"]]) / (.data[["B03_scaled"]] + .data[["B11_scaled"]]),
      
      # MNDWI (Xu, 2006 - using SWIR2/B12) = (Green - SWIR2) / (Green + SWIR2) => (B03 - B12) / (B03 + B12)
      mndwi12 = (.data[["B03_scaled"]] - .data[["B12_scaled"]]) / (.data[["B03_scaled"]] + .data[["B12_scaled"]]),
      
      # NDMI (Gao, 1996 - often called NDMI, using SWIR1/B11) = (NIR - SWIR1) / (NIR + SWIR1) => (B8A - B11) / (B8A + B11)
      ndmi_gao11 = (.data[["B8A_scaled"]] - .data[["B11_scaled"]]) / (.data[["B8A_scaled"]] + .data[["B11_scaled"]]),
      
      # STR (Soil Tillage Residue) index using your custom function
      # Assumes B11 and B12 columns contain raw integer S2 values that need scaling by 10000 for this index
      str1 = swir_to_str(.data[["B11"]]),
      str2 = swir_to_str(.data[["B12"]])
    )
  message("   Spectral indices calculated and added as new columns to 'combined_pixel_data_df'.")
}

# Display head with some of the new index columns
index_cols_to_check <- c("ndvi", "ndwi_mf", "mndwi11", "ndmi_gao11", "str1")
# Ensure these new columns actually exist before trying to print them
actual_cols_to_print <- index_cols_to_check[index_cols_to_check %in% names(combined_pixel_data_df)]
if(length(actual_cols_to_print) > 0) {
  message("INFO: First few rows of 'combined_pixel_data_df' with some of the new spectral indices:")
  print(head(combined_pixel_data_df[, actual_cols_to_print, drop = FALSE]))
} else {
  message("WARN: Could not find newly created index columns to print their head. Check calculation step.")
  print(head(combined_pixel_data_df)) # Print head of df anyway
}

# --- Part 6: Add final label and pureness ---

# 1. Identify the fraction columns as columns 3 to 6 of combined_pixel_data_df
message("INFO: Identifying fraction columns as columns 3 to 6 of 'combined_pixel_data_df'.")
if (ncol(combined_pixel_data_df) < 6) {
  stop(paste("ERROR: 'combined_pixel_data_df' has fewer than 6 columns (has", ncol(combined_pixel_data_df), "). Cannot select columns 2 to 5 for fractions."))
}
your_fraction_column_names <- names(combined_pixel_data_df)[3:6] 

if (length(your_fraction_column_names) != 4) { # From 2:5, there should be 4 columns
  stop(paste("ERROR: Attempt to select columns 2:5 for fractions did not result in 4 column names.",
             "This usually means ncol(combined_pixel_data_df) was exactly 5 and indexing needs care,",
             "or an issue with names() function. Actual names selected:", paste(your_fraction_column_names, collapse=", ")))
}
message(paste("INFO: Using columns named '", paste(your_fraction_column_names, collapse="', '"), "' as fraction columns for analysis.", sep=""))

# Sanity check: Ensure these selected fraction columns are numeric
are_fractions_numeric <- sapply(combined_pixel_data_df[, your_fraction_column_names, drop = FALSE], is.numeric)
if (!all(are_fractions_numeric)) {
  non_numeric_fraction_cols <- your_fraction_column_names[!are_fractions_numeric]
  stop(paste("ERROR: The following specified fraction columns (from cols 2:5) are not numeric:", 
             paste(non_numeric_fraction_cols, collapse = ", "),
             ". Fractional coverage columns must be numeric."))
}

pixel_fractions_subset_df <- combined_pixel_data_df[, your_fraction_column_names, drop = FALSE]

# 2. Define function to determine mixture level and base label (remains the same)
determine_dominant_and_mixture <- function(fraction_row_values, all_label_names) {
  fraction_row_values <- as.numeric(fraction_row_values)
  if (all(is.na(fraction_row_values))) {
    return(list(dominant_label = NA_character_, mixture_category = NA_character_))
  }
  max_fraction <- max(fraction_row_values, na.rm = TRUE)
  if (is.na(max_fraction) || !is.finite(max_fraction) || length(fraction_row_values) == 0) {
    return(list(dominant_label = NA_character_, mixture_category = NA_character_))
  }
  best_label_index <- which.max(fraction_row_values)
  current_dominant_label <- all_label_names[best_label_index] # Uses the actual column name as the label
  current_mixture_category <- NA_character_
  if (max_fraction >= 0.9) {
    current_mixture_category <- "pure"
  } else if (max_fraction > 0.6) {
    current_mixture_category <- "mixed"
  } else {
    current_mixture_category <- "very_mixed"
  }
  return(list(dominant_label = current_dominant_label, mixture_category = current_mixture_category))
}

# 3. Apply this function to each row of the pixel_fractions_subset_df
message("INFO: Applying mixture level and base label rules (using fractions from columns 2:5)...")
labeling_results_list <- apply(pixel_fractions_subset_df, 1, # 1 indicates row-wise
                               FUN = determine_dominant_and_mixture,
                               all_label_names = your_fraction_column_names) # Pass the actual column names

# 4. Extract these into two separate vectors and add as new columns
combined_pixel_data_df$dominant_label    <- sapply(labeling_results_list, function(res) res$dominant_label)
combined_pixel_data_df$mixture_category <- sapply(labeling_results_list, function(res) res$mixture_category)

if ("final_label" %in% names(combined_pixel_data_df)) {
  message("INFO: Removing old 'final_label' column.")
  combined_pixel_data_df$final_label <- NULL
}

message("INFO: 'dominant_label' and 'mixture_category' columns have been added/updated in 'combined_pixel_data_df'.")

# 5. Verify by printing names and head
message("INFO: Current column names in 'combined_pixel_data_df':")
print(names(combined_pixel_data_df))
message("INFO: First few rows of 'combined_pixel_data_df' with new label columns:")
# Show the first selected fraction column, dominant_label, and mixture_category for context
cols_to_check <- c(your_fraction_column_names[1], "dominant_label", "mixture_category") 
cols_to_check <- cols_to_check[cols_to_check %in% names(combined_pixel_data_df)] 
if (length(cols_to_check) > 0) {
  print(head(combined_pixel_data_df[, cols_to_check, drop = FALSE]))
} else {
  print(head(combined_pixel_data_df)) 
}

# Optional: Summaries of the new columns
if ("mixture_category" %in% names(combined_pixel_data_df)) {
  message("INFO: Summary of 'mixture_category' counts:")
  print(table(combined_pixel_data_df$mixture_category, useNA = "ifany"))
}
if ("dominant_label" %in% names(combined_pixel_data_df)) {
  message("INFO: Summary of 'dominant_label' counts:")
  print(table(combined_pixel_data_df$dominant_label, useNA = "ifany"))
}


# Create the directory if it doesn't exist
if (!dir.exists(output_csv_directory)) {
    message(paste("INFO: Creating directory for CSV output:", output_csv_directory ))
    dir.create(output_csv_directory , recursive = TRUE)
}

# Define the filename (using 'output_df_filename' from your script's Section 2 if available, or define new)
if (!exists("output_df_filename")) {
    output_df_filename <- paste0(study_site_name, "_", study_year, "_final_pixel_data_with_labels.csv")
}
final_csv_filepath <- file.path(output_csv_directory, output_df_filename)

message(paste("INFO: Attempting to save data frame to:", final_csv_filepath))

# Save the data frame using write.csv
# row.names = FALSE prevents R from writing an extra column with row numbers.
tryCatch({
  write.csv(combined_pixel_data_df, file = final_csv_filepath, row.names = FALSE, quote = TRUE)
  message(paste("INFO: Final data frame successfully saved to:", final_csv_filepath))
}, error = function(e) {
  warning(paste("WARNING: Could not save the data frame as a CSV file.",
                "Please check write permissions for the directory '", csv_output_dir, "'.",
                "\nError message:", e$message))
})



# --- Part 6: Create Boxplots for S2 Bands and indices by "Pure" Dominant Labels ---

# 2. Filter for "pure" mixture category
pure_dominant_labels_df <- combined_pixel_data_df[which(combined_pixel_data_df$mixture_category == "pure"), ]

if (nrow(pure_dominant_labels_df) == 0) {
  message("INFO: No pixels found with mixture_category == 'pure'. Skipping boxplot creation.")
} else {
  message(paste("INFO: Found", nrow(pure_dominant_labels_df), "pixels with 'pure' mixture category for boxplots."))
  pure_dominant_labels_df$dominant_label <- as.factor(pure_dominant_labels_df$dominant_label)
  
  # 3. Identify columns for boxplotting:
  #    a) SCALED Sentinel-2 bands (e.g., columns starting with "B" and ending with "_scaled")
  scaled_s2_band_plot_cols <- names(pure_dominant_labels_df)[
    startsWith(names(pure_dominant_labels_df), "B") & endsWith(names(pure_dominant_labels_df), "_scaled")
  ]
  
  #    b) Spectral Index columns (use the exact names you assigned them)
  index_plot_cols <- c("ndvi", "ndwi_mf", "mndwi11", "mndwi12", "ndmi_gao11", "str1", "str2")
  # Filter for index columns that actually exist in the dataframe
  index_plot_cols_exist <- index_plot_cols[index_plot_cols %in% names(pure_dominant_labels_df)]
  
  # Combine both sets of columns
  cols_for_boxplotting <- c(scaled_s2_band_plot_cols, index_plot_cols_exist)
  
  if (length(cols_for_boxplotting) == 0) {
    message("WARNING: No scaled S2 band or spectral index columns identified for boxplotting. Please check column names and previous calculation steps.")
  } else {
    message(paste("INFO: Variables selected for boxplots:", paste(cols_for_boxplotting, collapse=", ")))
    
    # 4. Reshape data to long format
    pure_dominant_labels_long_df <- tidyr::pivot_longer(
      pure_dominant_labels_df,
      cols = all_of(cols_for_boxplotting), # Use the combined list of columns
      names_to = "variable_name",          # New column for variable names (S2 bands or indices)
      values_to = "value"                  # New column for their values
    )
    
    # Remove rows where 'value' is NA or Inf/-Inf, as boxplot can't handle them well
    pure_dominant_labels_long_df <- pure_dominant_labels_long_df[
      !is.na(pure_dominant_labels_long_df$value) & is.finite(pure_dominant_labels_long_df$value), 
    ]
    
    if (nrow(pure_dominant_labels_long_df) > 0) {
      message("INFO: Data reshaped and filtered. Generating boxplots for scaled bands and indices...")
      
      # 5. Create boxplots
      # Ensure 'variable_name' is treated as a factor for consistent facet order if desired
      # pure_dominant_labels_long_df$variable_name <- factor(pure_dominant_labels_long_df$variable_name, levels = cols_for_boxplotting)
      
      boxplot_scaled_bands_and_indices <- ggplot(
        pure_dominant_labels_long_df, 
        aes(x = dominant_label, y = value, fill = dominant_label)
      ) +
        geom_boxplot(notch = FALSE) +
        facet_wrap(~ variable_name, scales = "free_y") + # Facet by each S2 band and Index
        labs(
          title = "Distribution of Scaled S2 Bands & Indices for 'Pure' Dominant Labels",
          x = "Dominant Label (Mixture Category: Pure)",
          y = "Value (Scaled Reflectance or Index)" # Generic y-axis label
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
          strip.text = element_text(size = 8), # Adjust facet title size if needed
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "none"
        ) +
        guides(fill="none")
      
      print(boxplot_scaled_bands_and_indices)
      
      # Save the plot
      # Ensure study_site_name, study_year, output_root_dir (and potentially output_csv_directory) are defined
      if (exists("study_site_name") && exists("study_year") && exists("output_root_dir")) {
        # Define your desired plot filename
        plot_filename_updated <- paste0(study_site_name, "_", study_year, "_scaled_s2_indices_boxplot_purepixels.png") # Using your example filename
        
        # Construct the path to use "boxplots" subfolder directly under output_root_dir
        plot_output_dir_base <- file.path(output_root_dir, 
                                          "boxplots", # Changed from "fraction rasters" or generic "plots"
                                          study_site_name, 
                                          study_year)
        
        if (!dir.exists(plot_output_dir_base)) {
          message(paste("INFO: Creating plot output directory:", plot_output_dir_base))
          dir.create(plot_output_dir_base, recursive = TRUE)
        }
        plot_filepath <- file.path(plot_output_dir_base, plot_filename_updated)
        
        tryCatch({
          ggsave(plot_filepath, plot = boxplot_scaled_bands_and_indices, width = 14, height = 10, dpi = 300) # Adjust dimensions as needed
          message(paste("INFO: Comprehensive boxplot saved to:", plot_filepath))
        }, error = function(e) {
          warning(paste("WARNING: Could not save comprehensive boxplot to '", plot_filepath, "'. Error: ", e$message, sep=""))
        })
      } else {
        message("INFO: Skipping plot saving as 'study_site_name', 'study_year', or 'output_root_dir' is not defined. Ensure these are set in your script's configuration section.")
      }
      
    } else { 
      message("WARNING: Data for boxplotting became empty after reshaping or NA/Inf filtering. Check input data.")
    }
  } 
} 

