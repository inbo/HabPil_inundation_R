# --- 1. Load Required R Packages ---
message("Loading required R packages...")
# Ensure these are installed: install.packages(c("sf", "dplyr", "googledrive", "tools"))
if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required but not installed.")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required but not installed.")
if (!requireNamespace("googledrive", quietly = TRUE)) stop("Package 'googledrive' is required but not installed.")
if (!requireNamespace("tools", quietly = TRUE)) stop("Package 'tools' is required but not installed.")

library(sf)
library(dplyr)
library(googledrive) 
library(tools)  

# --- 2. Source Utility and Configuration Files ---
message("Sourcing utility and configuration files...")
# Define paths to the utility and configuration files
# Assumed to be in a 'source' subdirectory relative to this main script.
gdrive_utils_path <- "source/gdrive_utils.R" 
spatial_utils_path <- "source/spatial_processing_utils.R" 
config_path <- "source/config.R"          

# Helper function to check and source a file
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

my_base_dir <- file.path(data_root_dir, "LabelMe", study_site_name, study_year)
if (!dir.exists(my_base_dir )) {
  message("Local cache directory '", my_base_dir , "' does not exist. Creating it.")
  dir.create(my_base_dir , recursive = TRUE, showWarnings = TRUE)
} else {
  message("Using existing local cache directory: ", my_base_dir )
}


# --- 3. Google Drive Authentication & Data Download ---
message("Authenticating with Google Drive...")
# The .gdrive_authenticated variable is defined and managed in gdrive_utils.R
if (!exists(".gdrive_authenticated") || !.gdrive_authenticated) {
  # Ensure authenticate_gdrive is found by checking if it's a function after sourcing
  if (!is.function(authenticate_gdrive)) {
    stop("Function 'authenticate_gdrive' not found. Check sourcing of gdrive_utils.R")
  }
  .gdrive_authenticated <- authenticate_gdrive() # Function from gdrive_utils.R
}
if (!.gdrive_authenticated) {
  stop("Google Drive authentication failed. Cannot proceed with downloads.")
}

# Define datasets_to_download_config using variables from config.R
# Ensure gdrive_id_labels, labels_filename, labels_subfolder_name,
# gdrive_id_tiles, tiles_filename, and tiles_subfolder_name are defined in config.R
datasets_to_download_config <- list(
  list(
    name = "PolygonsData", # This name is used as a key in the 'downloaded_paths' list
    gdrive_id = gdrive_id_labels, 
    target_filename = labels_filename, 
    local_subfolder = labels_subfolder_name 
  ),
  list(
    name = "TilesData",    # This name is used as a key in the 'downloaded_paths' list
    gdrive_id = gdrive_id_tiles, 
    target_filename = tiles_filename, 
    local_subfolder = tiles_subfolder_name 
  )
)

message("\n--- Starting GDrive Download Process ---")
# perform_project_data_acquisition is from gdrive_utils.R
# Ensure perform_project_data_acquisition is found
if (!is.function(perform_project_data_acquisition)) {
  stop("Function 'perform_project_data_acquisition' not found. Check sourcing of gdrive_utils.R")
}
downloaded_paths <- perform_project_data_acquisition(
  datasets_config = datasets_to_download_config,
  base_cache_dir = my_base_dir 
)
if (is.null(downloaded_paths) || length(downloaded_paths) == 0) { 
  stop("Data download process failed, was aborted, or returned no paths. Check previous messages.")
}
message("\n--- GDrive Download Process Completed ---")

# --- 5. Prepare File Paths and Parameters for Spatial Processing ---
message("Preparing inputs for spatial processing...")
input_labels_file_path <- NULL # Initialize path for labels/polygons
input_grid_file_path <- NULL # Initialize path for grid/tiles

# Construct full paths to the downloaded shapefiles using the results from perform_project_data_acquisition
if (!is.null(downloaded_paths$PolygonsData) && nzchar(downloaded_paths$PolygonsData)) {
  input_labels_file_path <- file.path(downloaded_paths$PolygonsData, labels_filename) # (from config.R)
  if (!file.exists(input_labels_file_path)) {
    stop("Labels shapefile not found after download attempt at: ", input_labels_file_path)
  }
  message("Path to labels shapefile (input_labels_file_path): ", input_labels_file_path)
} else {
  stop("Could not determine path for PolygonsData. Check 'name' in datasets_to_download_config and download logs.")
}

if (!is.null(downloaded_paths$TilesData) && nzchar(downloaded_paths$TilesData)) {
  input_grid_file_path <- file.path(downloaded_paths$TilesData, tiles_filename) # (from config.R)
  if (!file.exists(input_grid_file_path)) {
    stop("Tiles shapefile not found after download attempt at: ", input_grid_file_path)
  }
  message("Path to tiles shapefile (input_grid_file_path): ", input_grid_file_path)
} else {
  stop("Could not determine path for TilesData. Check 'name' in datasets_to_download_config and download logs.")
}

# Bundle configuration parameters needed by the spatial processing function
# Ensure background_label, tile_id_column_name are defined in config.R
processing_config_params <- list(
  background_label = background_label,
  tile_id_column_name = tile_id_column_name
)


# --- 6. Execute Spatial Data Processing ---
message("\n--- Calling Spatial Data Processing Workflow ---")
# The process_spatial_layers function is defined in spatial_processing_utils.R
# Ensure process_spatial_layers is found
if (!is.function(process_spatial_layers)) {
  stop("Function 'process_spatial_layers' not found. Check sourcing of spatial_processing_utils.R")
}
output_object_sf <- process_spatial_layers( 
  input_labels_path = input_labels_file_path,
  input_grid_path = input_grid_file_path,
  config_params = processing_config_params
)

if (is.null(output_object_sf) || !inherits(output_object_sf, "sf")) {
  stop("Spatial processing did not return a valid sf object. Check processing logs.")
}


# --- 7. Write Output ---
message("\n--- Preparing to Write Output ---")
output_gpkg_filename <- paste0(study_site_name, "_", study_year, 
                               "_final_labeled.gpkg") # Dynamic filename
output_file_path <- file.path(output_root_dir, "labeled", study_site_name, study_year, output_gpkg_filename)
message("Full output GPKG path will be: ", output_file_path)
# Ensure the output directory exists
output_dir <- dirname(output_file_path) 
if (!dir.exists(output_dir)) {
  message("Output directory '", output_dir, "' does not exist. Attempting to create it.")
  dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
}

# Write the final sf object to a GeoPackage
if (inherits(output_object_sf, "sf") && nrow(output_object_sf) > 0 && dir.exists(output_dir)) {
  message("Writing final result (", nrow(output_object_sf), " features) to: ", output_file_path)
  tryCatch({
    sf::st_write(output_object_sf, output_file_path, delete_layer = TRUE, quiet = FALSE)
    message("     - Write completed successfully.")
  }, error = function(e) {
    warning("Error writing final output file '", output_file_path, "': ", conditionMessage(e))
  })
} else if (!dir.exists(output_dir)){
  warning("Output directory '", output_dir, "' could not be found or created. Result NOT written.")
} else if (!inherits(output_object_sf, "sf")) {
  warning("Final object is not a valid sf object. Result NOT written.")
} else if (nrow(output_object_sf) == 0) {
  message("Final result is an sf object with 0 features. No output file written to: ", output_file_path)
} else {
  message("Final result is empty or invalid for other reasons. No output file written to: ", output_file_path)
}

message("\n--- Main Script Finished ---")
