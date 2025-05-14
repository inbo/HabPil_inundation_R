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


tiles_base_dir <- file.path(data_root_dir, "LabelMe", study_site_name, study_year)
tiles_shp_path <- file.path(tiles_base_dir, tiles_subfolder_name, tiles_filename)
labels_filename <- paste0(study_site_name, "_", study_year, "_final_labeled.gpkg")
labels_path <- file.path(output_root_dir, "labeled", study_site_name, study_year, labels_filename)
sen2_path <- file.path(data_root_dir, "Sen2", study_site_name, study_year)
sen2_filename <- paste0("Sen2_",study_site_name, "_", study_year, ".tif")

tile_filter_column <- tile_id_column_name
label_column <- "Label"


if (!dir.exists(dirname(tiles_shp_path))) {
  # You might want to stop, warn, or create it depending on workflow
  # stop(paste("Directory for tiles '", dirname(tiles_shp_path), "' does not exist.", sep=""))
  warning(paste("Directory for tiles '", dirname(tiles_shp_path), "' does not exist. File reading might fail.", sep=""))
  # Or create it: dir.create(dirname(tiles_shp_path), recursive = TRUE, showWarnings = TRUE)
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
  
  # Add check for empty strings "" if TileID is character
  # if (is.character(selected_tiles[[tile_filter_column]])) {
  #   selected_tiles <- selected_tiles %>% filter(.data[[tile_filter_column]] != "")
  # }
  
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
# Assign the CRS to be used for subsequent operations
target_crs <- tiles_crs

#---------------------------------------------------------------------
# Step 5: Define point generation parameters (Unchanged)
point_density <- 0.001
min_distance <- units::set_units(25, "m")

#---------------------------------------------------------------------
# Step 6: Generate points within the selected TILES (Unchanged logic, uses sf::)

message("\nStarting point generation within selected tiles...")
message(" -> Combining selected tiles (dissolve)...")
valid_tiles_geom <- sf::st_make_valid(sf::st_geometry(selected_tiles))
combined_tile_area_geom <- sf::st_union(valid_tiles_geom)
total_tile_area <- sf::st_area(combined_tile_area_geom)
total_tile_area_sqm <- units::set_units(total_tile_area, "m^2")
target_total_points <- floor(units::drop_units(total_tile_area_sqm * point_density))
message(paste(" -> Total area of selected tiles:", round(total_tile_area_sqm, 2)))
message(paste(" -> Target number of points within tiles:", target_total_points))
generated_points_in_tiles <- NULL
if (target_total_points > 0) {
  sampling_polygon_sf <- sf::st_sf(geometry = combined_tile_area_geom, crs = target_crs)
  generated_points_in_tiles <- sample_with_min_dist(
    polygons = sampling_polygon_sf, n_target = target_total_points, min_dist = min_distance
  )
} else {
  message(" -> Target number of points is 0, no points generated.")
  generated_points_in_tiles <- sf::st_sf(geometry = sf::st_sfc(crs = target_crs))
}

#---------------------------------------------------------------------
# Step 7: Load the LABELED POLYGONS data for assigning labels (Unchanged logic, uses sf::)

points_with_labels <- generated_points_in_tiles
if (!is.null(generated_points_in_tiles) && nrow(generated_points_in_tiles) > 0) {
  # File existence checks
  if (!dir.exists(dirname(labels_path))) { stop(paste("Directory does not exist:", dirname(labels_path))) }
  if (!file.exists(labels_path)) { stop(paste("File does not exist:", labels_path)) }
  message(paste("\nLoading labeled polygons from:", labels_path))
  
  tryCatch({ 
    # Load labeled polygons
    labeled_polygons <- sf::st_read(labels_path) # Add layer = labeled_layer_name if needed
    message("Labeled polygons loaded successfully.")
    
    message("DEBUG: Column names in your labeled GeoPackage are:")
    print(names(labeled_polygons))
    
    # Check label column
    if (!label_column %in% names(labeled_polygons)) {
      stop(paste("Label column '", label_column, "' not found in ", basename(labels_path), ".", sep=""))
    }
    
    # Check and potentially transform CRS
    labeled_crs <- sf::st_crs(labeled_polygons)
    if (is.na(labeled_crs)) {
      stop(paste("Labeled polygons file", basename(labels_path), "missing CRS."))
    }
    if (labeled_crs != target_crs) {
      message(paste("Labeled polygons CRS differs. Transforming labeled polygons..."))
      labeled_polygons <- sf::st_transform(labeled_polygons, crs = target_crs)
      message("Transformation complete.")
    }
    
    # Prepare data for join
    labels_for_join <- labeled_polygons %>%
      select(all_of(label_column)) %>%
      sf::st_make_valid()
    
    # Spatial Join
    message(" -> Assigning labels to points via spatial join...")
    points_with_labels <- sf::st_join(
      generated_points_in_tiles,
      labels_for_join,
      join = sf::st_intersects,
      left = TRUE
    )
    message(" -> Join completed.")
    
    # Clean up geometry columns if necessary
    if("geometry.x" %in% names(points_with_labels)) { points_with_labels <- points_with_labels %>% rename(geometry = geometry.x) }
    if("geometry.y" %in% names(points_with_labels)) { points_with_labels <- points_with_labels %>% select(-geometry.y) }
    
    # Print results
    print(paste("Total generated points:", nrow(points_with_labels)))
    print("Sample:"); print(head(points_with_labels))
    print("Points per label:"); points_with_labels[[label_column]] <- as.factor(points_with_labels[[label_column]]); print(table(points_with_labels[[label_column]], useNA = "ifany"))
    
    # Plot results
    message("Generating plot...")
    plot_obj <- ggplot() +
      geom_sf(data = sf::st_geometry(selected_tiles), fill = "gray80", color = "gray50", alpha = 0.3) +
      geom_sf(data = points_with_labels, aes(color = .data[[label_column]]), size = 0.5) +
      scale_color_discrete(name = "Assigned Label", na.value = "black") +
      ggtitle(paste("Points Generated (Tiles where", tile_id_column_name, "not NA, Labeled)")) +
      labs(caption = paste("Study Site:", study_site_name, "| Year:", study_year)) +
      theme_minimal()
    print(plot_obj)
    
    #Save - Construct path using config variables
    output_points_dir <- file.path(output_root_dir, "points", study_site_name, study_year)
    output_points_filename <- paste0(study_site_name, "_", study_year, "_tileID_sampled_points.gpkg")
    output_points_path <- file.path(output_points_dir, output_points_filename)
    if (!dir.exists(dirname(output_points_path))) { dir.create(dirname(output_points_path), recursive = TRUE) }
    message(paste("Saving final points to:", output_points_path))
    sf::st_write(points_with_labels, output_points_path, layer = "tileID_sampled_points_50m", driver = "GPKG", delete_layer = TRUE)
    message("Save complete.")
    
  }, error = function(e) { # CORRECTED: Use 'error = function(e)' instead of 'catch (e)'
    # This block executes if any error occurs within the main tryCatch block above
    stop(paste("Error during labeling step:", e$message))
  }) # End of tryCatch call
  
} else {
  message("\nNo points were generated, skipping labeling.")
}

# =============================================================
# Step 9: Extract Sentinel-2 Raster Values at Point Locations
# =============================================================
if (!exists("sen2_path") || !exists("sen2_filename")) {
  stop(
    "Variables 'sen2_path' and/or 'sen2_filename' are not defined. ",
    "Please ensure these are set (e.g., from your config.R file) before running Step 9."
  )
}
sentinel2_raster_full_path <- file.path(sen2_path, sen2_filename)

# Check if sentinel2_raster_full_path exists
if (!file.exists(sentinel2_raster_full_path)) {
  stop(
    paste("Sentinel-2 raster file does not exist at the constructed path:", sentinel2_raster_full_path),
    "\nPlease check 'sen2_path' and 'sen2_filename' definitions and file location."
  )
}

# --- Load Sentinel-2 Raster ---
message(paste("\nLoading Sentinel-2 raster from:", sentinel2_raster_full_path))
tryCatch({
  s2_raster <- terra::rast(sentinel2_raster_full_path)
  message("Sentinel-2 raster loaded successfully.")
  print(s2_raster) # Print some raster info
}, error = function(e) {
  stop(paste("Error loading Sentinel-2 raster:", e$message))
})

# --- Check and Match CRS between Points and Raster ---
message("Checking CRS of points and raster...")
if (!inherits(points_with_labels, "sf")) {
  stop("Object 'points_with_labels' is not an sf object. Cannot proceed.")
}
points_current_crs <- sf::st_crs(points_with_labels)
raster_current_crs <- terra::crs(s2_raster)

points_for_extraction <- points_with_labels # Assume no transformation needed initially
if (!terra::same.crs(points_with_labels, s2_raster)) {
  message(paste("CRS mismatch detected."))
  message(paste("  Points CRS:", points_current_crs$input %||% "Unknown"))
  message(paste("  Raster CRS:", raster_current_crs %||% "Unknown")) # terra::crs() returns WKT
  message("Transforming points to match raster CRS for extraction...")
  points_for_extraction <- sf::st_transform(points_with_labels, terra::crs(s2_raster))
  message(paste("Points transformed to:", sf::st_crs(points_for_extraction)$input %||% "Unknown"))
} else {
  message("Points and raster CRS match.")
}

# --- Extract Raster Values ---
if (nrow(points_for_extraction) == 0) {
  message("No points available for raster value extraction. Skipping extraction.")
  points_with_s2_values <- points_for_extraction # Will be an empty sf object
} else {
  message("Extracting raster values at point locations...")
  tryCatch({
    points_with_s2_values <- terra::extract(s2_raster, points_for_extraction, bind = TRUE, ID = FALSE)
    message("Raster values extracted and combined with point data.")
    
    original_cols <- names(points_with_labels) # Use original point data for comparison
    newly_added_cols <- names(points_with_s2_values)[!names(points_with_s2_values) %in% original_cols]
    
    # Robust way to find new columns if points_for_extraction was a transformed copy
    if (length(newly_added_cols) == 0 || all(newly_added_cols %in% names(points_with_labels))) {
      attr_cols_original <- names(sf::st_drop_geometry(points_with_labels))
      all_cols_new <- names(sf::st_drop_geometry(points_with_s2_values))
      newly_added_cols <- all_cols_new[!all_cols_new %in% attr_cols_original]
    }
    
    message("Sample of points with extracted Sentinel-2 values:")
    print(head(points_with_s2_values))
    message(paste("Number of new columns added (raster bands):", length(newly_added_cols)))
    if(length(newly_added_cols) > 0) {
      message("Names of new columns for Sentinel-2 bands:")
      print(newly_added_cols)
    } else {
      message("No new columns appear to have been added, or they have the same names as existing columns. Please check band names in the raster and output carefully.")
    }
    
  }, error = function(e) {
    stop(paste("Error during raster value extraction:", e$message))
  })
}

# Save the points with extracted Sentinel-2 values
if (exists("output_root_dir") && exists("study_site_name") && exists("study_year")) {
  output_s2_points_dir <- file.path(output_root_dir, "points", study_site_name, study_year) # Suggesting a more descriptive subfolder
  if (!dir.exists(output_s2_points_dir)) {
    dir.create(output_s2_points_dir, recursive = TRUE, showWarnings = TRUE)
  }
  output_s2_points_filename <- paste0(study_site_name, "_", study_year, "_points_with_s2_values.gpkg")
  output_s2_points_path <- file.path(output_s2_points_dir, output_s2_points_filename)
  
  if (exists("points_with_s2_values") && !is.null(points_with_s2_values) && nrow(points_with_s2_values) > 0) {
    message(paste("Preparing to save final points with Sentinel-2 values to:", output_s2_points_path))
    
    points_to_save_sf <- NULL
    if (inherits(points_with_s2_values, "SpatVector")) {
      message("Object 'points_with_s2_values' is a SpatVector. Converting to sf object for saving...")
      tryCatch({
        points_to_save_sf <- sf::st_as_sf(points_with_s2_values)
        message("Conversion to sf object successful.")
      }, error = function(e) {
        stop(paste("Error converting SpatVector to sf object:", e$message))
      })
    } else if (inherits(points_with_s2_values, "sf")) {
      points_to_save_sf <- points_with_s2_values # Already an sf object
      message("Object 'points_with_s2_values' is already an sf object.")
    } else {
      stop(paste("Object 'points_with_s2_values' is of unexpected class:", class(points_with_s2_values)[1], ". Cannot save with st_write."))
    }
    
    # Proceed with saving only if conversion was successful or it was already sf
    if (!is.null(points_to_save_sf)) {
      message("Saving data...")
      sf::st_write(points_to_save_sf, output_s2_points_path, layer = "points_with_s2_values", driver = "GPKG", delete_layer = TRUE)
      message("Save complete.")
    } else {
      warning("Could not prepare 'points_to_save_sf' for saving.")
    }
    
  } else {
    message("No points with S2 values to save (or 'points_with_s2_values' object not created, is NULL, or is empty).")
  }
} else {
  message("Config variables (output_root_dir, study_site_name, study_year) not found; skipping save of points with S2 values.")
}

message("\nStep 9: Raster value extraction finished.")



message("\nScript finished.")