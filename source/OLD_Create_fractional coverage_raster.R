# Step 1: Check and Install Required Packages Automatically
# --- 1. Load Required R Packages ---
message("Loading required R packages...")
# Ensure these are installed: install.packages(c("sf", "dplyr", "units", "ggplot2"))
if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required but not installed.")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required but not installed.")
if (!requireNamespace("googledrive", quietly = TRUE)) stop("Package 'googledrive' is required but not installed.")
if (!requireNamespace("tools", quietly = TRUE)) stop("Package 'tools' is required but not installed.")
if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required but not installed.")


library(sf)
library(dplyr)
library(units) 
library(ggplot2)  
library(terra)


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


tiles_base_dir <- file.path(data_root_dir, "LabelMe", study_site_name, study_year)
tiles_shp_path <- file.path(tiles_base_dir, tiles_subfolder_name, tiles_filename)
labels_filename <- paste0(study_site_name, "_", study_year, "_final_labeled.gpkg")
labels_path <- file.path(output_root_dir, "labeled", study_site_name, study_year, labels_filename)
sen2_path <- file.path(data_root_dir, "Sen2", study_site_name, study_year)
sen2_filename <- paste0("Sen2_",study_site_name, "_", study_year, ".tif")

tile_filter_column <- tile_id_column_name
label_column <- "Label"


if (!dir.exists(dirname(tiles_shp_path))) {
  warning(paste("Directory for tiles '", dirname(tiles_shp_path), "' does not exist. File reading might fail.", sep=""))
}

message(paste("Loading tiles from:", tiles_shp_path))
tryCatch({
  all_tiles <- st_read(tiles_shp_path)
  message("Tiles loaded successfully.")
  
  # Check if the filter column (TileID) exists
  if (!tile_filter_column %in% names(all_tiles)) {
    stop(paste("Tile filter column '", tile_filter_column, "' (from config variable 'tile_id_column_name') not found in ", basename(tiles_shp_path), ".", sep=""))
  }
  
  # Filter tiles: Keep only rows where the TileID column is NOT NA
  selected_tiles <- all_tiles %>%
    filter(!is.na(.data[[tile_filter_column]]))
  
  if (nrow(selected_tiles) == 0) {
    stop(paste("No tiles found in '", basename(tiles_shp_path), "' matching the filter condition: '", tile_filter_column, "' is not NA.", sep=""))
  }
  message(paste("Filtered tiles: found", nrow(selected_tiles), "tiles where", tile_filter_column, "is not NA."))
  
}, error = function(e) {
  stop(paste("Error loading or filtering tiles Shapefile:", e$message))
})


message("Checking CRS for selected tiles...")
tiles_crs <- st_crs(selected_tiles)
print(paste("Tiles CRS:", tiles_crs$input))

if (is.na(tiles_crs)) {
  stop("Tiles CRS is NA. Cannot proceed without valid CRS information.")
} else if (sf::st_is_longlat(tiles_crs)) { # MODIFIED CHECK: Stop if it IS Long/Lat
  stop("Tiles CRS is geographic (long/lat). This script requires a projected CRS (with units like meters). Please transform your tiles.")
} else {
  # If it's not NA and not Long/Lat, assume it's projected for the purpose of this script
  message("Tiles CRS appears to be projected (it is not long/lat).")
}


# Check units (only makes sense for projected CRS)
tiles_crs_units <- tiles_crs$units
if (!is.null(tiles_crs_units) && tiles_crs_units != "m") {
  warning(paste("Tiles CRS units might not be meters ('", tiles_crs_units, "'). Minimum distance/density might be incorrect.", sep=""))
} else if (is.null(tiles_crs_units)) {
  warning("Could not automatically check Tile CRS units. Assuming meters, but please verify.")
}


# --- 3. Load Sentinel-2 Data (Keeping its Original CRS) ---
message("INFO: Starting Step 3 - Load Sentinel-2 data (keeping its original CRS).")

# Check if the necessary path variables exist
if (!exists("sen2_path") || !exists("sen2_filename")) {
  stop(
    "ERROR: Variables 'sen2_path' and/or 'sen2_filename' are not defined. ",
    "Please ensure these are set (e.g., from your config.R file) before running Step 3."
  )
}

# Full path to the Sentinel-2 file
sen2_full_path <- file.path(sen2_path, sen2_filename)
message(paste("INFO: Loading Sentinel-2 data from:", sen2_full_path))

# Check if the Sentinel-2 raster file exists at the constructed path
if (!file.exists(sen2_full_path)) {
  stop(
    paste("ERROR: Sentinel-2 raster file not found at the constructed path:", sen2_full_path),
    "\nPlease check the definitions of 'sen2_path' and 'sen2_filename' and the file location."
  )
}

# Try to load the Sentinel-2 raster
tryCatch({
  sen2_raster <- terra::rast(sen2_full_path) # This will be our reference raster, not reprojected
  message("INFO: Sentinel-2 data loaded successfully in its original CRS.")
  message("INFO: Basic information about the loaded Sentinel-2 raster:")
  print(sen2_raster) 
}, error = function(e) {
  stop(paste("ERROR loading the Sentinel-2 raster:", e$message))
})


# Store the original CRS of the Sentinel-2 raster
s2_original_crs_wkt <- terra::crs(sen2_raster, proj = FALSE) # Get WKT
s2_original_crs_sf <- sf::st_crs(s2_original_crs_wkt)       # Convert to sf::crs object for st_transform
message(paste("INFO: Sentinel-2 raster original CRS (WKT):", s2_original_crs_wkt))

# --- 4. Load and Prepare Labeled Polygons (Transform to S2 CRS) ---
message("INFO: Starting Step 4 - Load and prepare labeled polygons, transforming to Sentinel-2 CRS.")
message(paste("INFO: Loading labeled polygons from:", labels_path))

if (!file.exists(labels_path)) {
  stop(paste("ERROR: Labeled polygons file not found at:", labels_path,
             ". Please check 'labels_path' in your config.R."))
}

# Try to load the labeled polygons
tryCatch({
  labeled_polygons_raw <- sf::st_read(labels_path)
  message("INFO: Labeled polygons loaded successfully.")
  
  if (!label_column %in% names(labeled_polygons_raw)) {
    stop(paste0("ERROR: Label column '", label_column, "' (from config variable 'label_column') ",
                "not found in '", basename(labels_path), "'."))
  }
  if (nrow(labeled_polygons_raw) == 0) {
    stop(paste("ERROR: No polygons found in", basename(labels_path)))
  }
}, error = function(e) {
  stop(paste("ERROR loading the labeled polygons:", e$message))
})

# Check and transform CRS of labeled polygons to match Sentinel-2 CRS
labels_crs_original_sf <- sf::st_crs(labeled_polygons_raw)
message(paste("INFO: Original CRS of labeled polygons:", labels_crs_original_sf$input))

if (is.na(labels_crs_original_sf)) {
  stop(paste("ERROR: CRS of labeled polygons ('", basename(labels_path), "') is NA. ",
             "A valid CRS is required.", sep=""))
}

if (labels_crs_original_sf != s2_original_crs_sf) {
  message(paste0("INFO: CRS of labeled polygons ('", labels_crs_original_sf$input,
                 "') differs from Sentinel-2 CRS ('", s2_original_crs_sf$input,
                 "'). Transforming labeled polygons to Sentinel-2 CRS..."))
  tryCatch({
    labeled_polygons <- sf::st_transform(labeled_polygons_raw, s2_original_crs_sf)
    message("INFO: Labeled polygons successfully transformed to Sentinel-2 CRS.")
    message(paste("INFO: New CRS of labeled polygons:", sf::st_crs(labeled_polygons)$input))
  }, error = function(e) {
    stop(paste("ERROR transforming labeled polygons to Sentinel-2 CRS:", e$message))
  })
} else {
  labeled_polygons <- labeled_polygons_raw # No transformation needed
  message("INFO: CRS of labeled polygons already matches Sentinel-2 CRS.")
}

# Validate and repair geometries if necessary (after potential transformation)
if (!all(sf::st_is_valid(labeled_polygons))) {
  message("WARNING: Invalid geometries found in labeled polygons. Attempting to repair with st_make_valid()...")
  labeled_polygons <- sf::st_make_valid(labeled_polygons)
  if (!all(sf::st_is_valid(labeled_polygons))) {
    message("WARNING: Not all geometries could be repaired. This might cause issues during rasterization.")
  } else {
    message("INFO: Geometries successfully repaired.")
  }
}

expected_geom_types <- c("POLYGON", "MULTIPOLYGON")
actual_geom_types <- unique(as.character(sf::st_geometry_type(labeled_polygons)))
if (!all(actual_geom_types %in% expected_geom_types)) {
  warning(paste("WARNING: Labeled polygons contain geometry types other than POLYGON/MULTIPOLYGON:",
                paste(actual_geom_types[!actual_geom_types %in% expected_geom_types], collapse=", "),
                ". These might be ignored or cause errors during rasterization."))
}

# --- 5. Transform Selected Tiles and Select Sentinel-2 Pixels ---
message("INFO: Starting Step 5 - Transform 'selected_tiles' to Sentinel-2 CRS and select overlapping S2 pixels.")

# 'selected_tiles' are assumed to be in 'target_crs' from the initial part of your script.
# Transform 'selected_tiles' to the Sentinel-2 raster's original CRS.
current_tiles_crs <- sf::st_crs(selected_tiles)
message(paste("INFO: Original CRS of 'selected_tiles':", current_tiles_crs$input))

if (current_tiles_crs != s2_original_crs_sf) {
  message(paste0("INFO: Transforming 'selected_tiles' from CRS '", current_tiles_crs$input,
                 "' to Sentinel-2 CRS ('", s2_original_crs_sf$input, "')..."))
  tryCatch({
    selected_tiles_s2crs <- sf::st_transform(selected_tiles, s2_original_crs_sf)
    message("INFO: 'selected_tiles' successfully transformed to Sentinel-2 CRS.")
  }, error = function(e) {
    stop(paste("ERROR transforming 'selected_tiles' to Sentinel-2 CRS:", e$message))
  })
} else {
  selected_tiles_s2crs <- selected_tiles # No transformation needed
  message("INFO: 'selected_tiles' are already in Sentinel-2 CRS.")
}

message("INFO: Unifying 'selected_tiles_s2crs' geometries...")
tryCatch({
  combined_tiles_geom_s2crs <- sf::st_union(selected_tiles_s2crs)
  combined_tiles_vect_s2crs <- terra::vect(combined_tiles_geom_s2crs)
  message("INFO: 'selected_tiles_s2crs' successfully unified.")
}, error = function(e){
  stop(paste("ERROR unifying 'selected_tiles_s2crs':", e$message))
})

message("INFO: Cropping original Sentinel-2 raster to the extent of unified (transformed) tiles...")
tryCatch({
  sen2_cropped <- terra::crop(sen2_raster, combined_tiles_vect_s2crs, snap="out")
  message("INFO: Original Sentinel-2 raster successfully cropped.")
}, error = function(e) {
  stop(paste("ERROR cropping the original Sentinel-2 raster:", e$message))
})

message("INFO: Masking original Sentinel-2 raster with the exact geometry of unified (transformed) tiles...")
tryCatch({
  s2_selected_pixels <- terra::mask(sen2_cropped, combined_tiles_vect_s2crs)
  message("INFO: Original Sentinel-2 raster successfully masked.")
  
  if (terra::ncell(s2_selected_pixels) == 0 || all(is.na(terra::values(s2_selected_pixels[[1]], mat = FALSE)))) {
    stop(paste("ERROR: No Sentinel-2 pixels found overlapping the (transformed) 'selected_tiles' after cropping and masking.",
               "Possible causes: tiles fall outside S2 image, CRS issues, or S2 image is empty/cloudy in this area."))
  }
  message("INFO: Sentinel-2 pixels (original grid) overlapping (transformed) 'selected_tiles' have been prepared.")
  message(paste("INFO: CRS of 's2_selected_pixels':", terra::crs(s2_selected_pixels, proj=TRUE)))
  
}, error = function(e) {
  stop(paste("ERROR masking the original Sentinel-2 raster:", e$message))
})


# --- Step 6 Calculate Per-Pixel Fractional Label Coverage ---
message("INFO: Starting Step 6 - Calculate Per-Pixel Fractional Label Coverage.")

if (!is.factor(labeled_polygons[[label_column]])) {
  labeled_polygons[[label_column]] <- as.factor(labeled_polygons[[label_column]])
}
unique_labels <- levels(labeled_polygons[[label_column]])
message(paste("INFO: Found unique labels for coverage analysis:", paste(unique_labels, collapse=", ")))

# Create a template raster from the s2_selected_pixels grid (first band is sufficient for grid)
# This template defines the pixels for which we want to calculate coverage.
s2_pixel_template <- terra::rast(s2_selected_pixels[[1]]) # Use first band for template geometry
terra::values(s2_pixel_template) <- NA # Clear values, we only need the grid

fractional_coverage_rasters <- list() # To store each label's coverage raster

message("INFO: Calculating fractional coverage for each label...")
for (current_label in unique_labels) {
  message(paste("INFO: Processing label:", current_label))
  
  # Filter polygons for the current label
  polygons_one_label_sf <- labeled_polygons[labeled_polygons[[label_column]] == current_label, ]
  
  if (nrow(polygons_one_label_sf) > 0) {
    polygons_one_label_vect <- terra::vect(polygons_one_label_sf)
    
    # Rasterize with cover=TRUE. Pixel values are fraction of cell covered.
    # background=0 means cells not covered by these specific polygons get 0 for this label.
    coverage_raster_one_label <- terra::rasterize(polygons_one_label_vect, s2_pixel_template, 
                                                  cover = TRUE, background = 0)
    
    
    # Mask again by s2_selected_pixels to ensure we only have values where original S2 data was valid
    # (if s2_selected_pixels[[1]] could have NAs within the extent from original S2 processing e.g. clouds)
    # This makes sense if s2_selected_pixels[[1]] itself has NAs for non-target pixels within its extent.
    # If s2_selected_pixels is already perfectly masked (no NAs for valid pixels), this might be redundant.
    # For safety, and if s2_selected_pixels might have internal NAs:
    coverage_raster_one_label <- terra::mask(coverage_raster_one_label, s2_selected_pixels[[1]])
    
    names(coverage_raster_one_label) <- as.character(current_label) # Name the layer
    fractional_coverage_rasters[[as.character(current_label)]] <- coverage_raster_one_label
  } else {
    message(paste("WARNING: No polygons found for label '", current_label, "'. Coverage for this label will be 0 everywhere.", sep=""))
    # Create a raster of zeros for this label if no polygons exist
    zero_coverage_raster <- s2_pixel_template
    terra::values(zero_coverage_raster) <- 0
    # Mask it as well for consistency
    zero_coverage_raster <- terra::mask(zero_coverage_raster, s2_selected_pixels[[1]])
    names(zero_coverage_raster) <- as.character(current_label)
    fractional_coverage_rasters[[as.character(current_label)]] <- zero_coverage_raster
  }
}

# Combine all individual label coverage rasters into one multi-layer SpatRaster
if (length(fractional_coverage_rasters) > 0) {
  all_coverage_rasters <- terra::rast(fractional_coverage_rasters)
  message("INFO: Multi-layer fractional coverage raster created successfully.")
  print(all_coverage_rasters)
  
} else {
  stop("ERROR: No fractional coverage rasters were generated. Check unique labels and polygon data.")
}

# Only keep Sen-2 pixels that are completely covered by labels (sum of fractions = 1)
if (length(fractional_coverage_rasters) > 0 && exists("all_coverage_rasters")) {
  message("INFO: Multi-layer fractional coverage raster created successfully.")
  print(all_coverage_rasters)
  
  # --- Sub-Step: Filter for Sentinel-2 Pixels Completely Covered by Labels ---
  message("INFO: Starting Sub-Step - Filtering for Sentinel-2 pixels completely covered by labels.")
  
  # Calculate the sum of fractional coverages across all label layers for each pixel
  # na.rm=TRUE is important if any layer could have NAs not aligned with others,
  # though our mask(..., s2_selected_pixels[[1]]) should have aligned NAs.
  total_coverage_per_pixel <- terra::app(all_coverage_rasters, fun = "sum", na.rm = TRUE)
  names(total_coverage_per_pixel) <- "total_label_coverage"
  
  message("INFO: Calculated total label coverage per pixel.")
  # print(summary(total_coverage_per_pixel)) # Optional: to see range of sums
  
  # Create a mask for pixels that are completely covered.
  # Direct comparison with 1.0 can be problematic due to floating-point precision.
  # It's often safer to use a small tolerance (epsilon).
  epsilon <- 1e-5 # A small tolerance, e.g., 0.001%
  # Pixels are considered completely covered if the sum of fractions is within epsilon of 1.0
  completely_covered_mask <- (total_coverage_per_pixel >= (1.0 - epsilon)) & (total_coverage_per_pixel <= (1.0 + epsilon))
  # Alternative for a stricter check if precision is very high:
  # completely_covered_mask <- total_coverage_per_pixel == 1.0
  
  names(completely_covered_mask) <- "is_completely_covered"
  message("INFO: Created mask for completely covered pixels.")
  # message(paste("INFO: Number of S2 pixels completely covered by labels:", terra::global(completely_covered_mask, "sum", na.rm=TRUE)$sum)) # Number of TRUE pixels
  
  # Apply the mask to the 'all_coverage_rasters'.
  # Pixels in 'all_coverage_rasters' that are NOT completely covered (i.e., mask is 0 or NA) will be set to NA.
  s2_pixels_fully_covered_by_labels <- terra::mask(all_coverage_rasters, 
                                                   completely_covered_mask, 
                                                   maskvalues = c(0, NA), # if mask is FALSE (0) or NA, it results in NA
                                                   updatevalue = NA) 
  
  message("INFO: Filtered coverage raster to keep only completely covered S2 pixels.")
  print(s2_pixels_fully_covered_by_labels)
  
  # Save this final filtered multi-layer raster
  output_filtered_raster_filename <- paste0(study_site_name, "_", study_year, "_fractions.tif")
  output_filtered_raster_path <- file.path(output_root_dir, "fraction rasters", study_site_name, study_year, output_filtered_raster_filename) # Adjust path
  if (!dir.exists(dirname(output_filtered_raster_path))) {
    dir.create(dirname(output_filtered_raster_path), recursive=TRUE) 
    if (terra::nlyr(s2_pixels_fully_covered_by_labels) > 0 && terra::ncell(s2_pixels_fully_covered_by_labels) > 0 && !all(is.na(terra::global(s2_pixels_fully_covered_by_labels[[1]], "sum", na.rm=TRUE)$sum))) {
      terra::writeRaster(s2_pixels_fully_covered_by_labels, output_filtered_raster_path, overwrite=TRUE)
      message(paste("INFO: Filtered fractional coverage raster (only fully covered S2 pixels) saved to:", output_filtered_raster_path))
      } else {
        message("INFO: The filtered raster 's2_pixels_fully_covered_by_labels' is empty or all NA, not saving.")
      }
  }
  
} else {
  # This part of the 'if' handles the case where 'all_coverage_rasters' wasn't created successfully.
  if (!exists("fractional_coverage_rasters") || length(fractional_coverage_rasters) == 0) {
    message("ERROR: No fractional coverage rasters were generated in the previous step. Cannot filter for completely covered pixels.")
  } else if (!exists("all_coverage_rasters")) {
    message("ERROR: 'all_coverage_rasters' object does not exist. Cannot filter for completely covered pixels.")
  }
}

# --- Step 7: Assign Dominant Label and Mixture Category ---
message("INFO: Part 1 - Assigning 'dominant_label' and 'mixture_category'.")

# --- User specified: Fraction columns are columns 3 to 6 ---
your_fraction_column_names <- names(combined_pixel_data_df)[3:6] 
message(paste("INFO: Using columns 2 to 5 as fraction columns, names are:", 
              paste(your_fraction_column_names, collapse=", ")))

if (length(your_fraction_column_names) != 4) { 
  stop("ERROR: Attempt to select columns 2:5 did not result in 4 column names. Check ncol(combined_pixel_data_df).")
}

# Ensure these columns are numeric, otherwise max() and other operations will fail
# This is a good place for a sanity check on the data type of these fraction columns
are_fractions_numeric <- sapply(combined_pixel_data_df[, your_fraction_column_names, drop = FALSE], is.numeric)
if (!all(are_fractions_numeric)) {
  non_numeric_fraction_cols <- your_fraction_column_names[!are_fractions_numeric]
  stop(paste("ERROR: The following specified fraction columns are not numeric:", 
             paste(non_numeric_fraction_cols, collapse = ", "),
             ". Fractional coverage columns must be numeric."))
}


pixel_fractions_subset_df <- combined_pixel_data_df[, your_fraction_column_names, drop = FALSE]

# Define a function to determine dominant label and mixture category
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
  current_dominant_label <- all_label_names[best_label_index]
  current_mixture_category <- NA_character_
  
  if (max_fraction >= 0.9) {
    current_mixture_category <- "pure"
  } else if (max_fraction >= 0.6) {
    current_mixture_category <- "mixed"
  } else {
    current_mixture_category <- "very_mixed"
  }
  return(list(dominant_label = current_dominant_label, mixture_category = current_mixture_category))
}

# Apply the function row-wise
message("INFO: Applying labeling rules to each pixel...")
labeling_results_list <- apply(pixel_fractions_subset_df, 1,
                               FUN = determine_dominant_and_mixture,
                               all_label_names = your_fraction_column_names) # Pass the actual column names as labels

# Add new columns to the data frame
combined_pixel_data_df$dominant_label    <- sapply(labeling_results_list, function(res) res$dominant_label)
combined_pixel_data_df$mixture_category <- sapply(labeling_results_list, function(res) res$mixture_category)

message("INFO: 'dominant_label' and 'mixture_category' columns added.")


