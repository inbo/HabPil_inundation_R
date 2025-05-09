# Load required R packages
library(sf)
library(dplyr)
library(googledrive)
library(tools)

# --- Configuration ---
source("source/gdrive_utils.R") 
source("source/config.R") 

# --- Input and Output Files ---

# Global variable to track Google Drive authentication status within the session
#.gdrive_authenticated <- FALSE

if (!exists(".gdrive_authenticated") || !.gdrive_authenticated) {
  .gdrive_authenticated <- authenticate_gdrive()
}


datasets_to_download_config <- list(
  list(
    name = "PolygonsData", # Key for the returned list of paths
    gdrive_id = gdrive_id_labels, 
    target_filename = target_filename_labels,
    local_subfolder = polygons_subfolder_variable 
    ),
  list(
    name = "TilesData",    # Key for the returned list of paths
    gdrive_id = gdrive_id_tiles, 
    target_filename = target_filename_tiles,
    local_subfolder = tiles_subfolder_variable
    )
  )

# --- Execute Downloads ---
message("\n--- Starting GDrive Download Process ---")

# Download the shapefile components for labels and tiles
downloaded_paths <- perform_project_data_acquisition(
  datasets_config = datasets_to_download_config,
  base_cache_dir = my_base_dir
  )

message("\n--- GDrive Download Process Completed ---")
message("Check the specified subdirectories within '", my_base_dir, "' for your downloaded files.")



# Reuse the paths of the downloaded shapefiles:
if (!is.null(downloaded_paths) && !is.null(downloaded_paths$PolygonsData)) {
  input_file_path <- file.path(downloaded_paths$PolygonsData, target_filename_labels)
  message("Path to polygons_py.shp: ", input_file_path)
  }

if (!is.null(downloaded_paths) && !is.null(downloaded_paths$TilesData)) {
  grid_boundary_file_path <- file.path(downloaded_paths$TilesData, target_filename_tiles)
  message("Path to Tiles_sel.shp: ", grid_boundary_file_path)
  }


# --- Function for safe geometry repair ---
safe_make_valid <- function(sf_object) {
  is_sf <- inherits(sf_object, "sf")
  is_sfc <- inherits(sf_object, "sfc")
  if (!is_sf && !is_sfc) {
    message("Warning: Input for safe_make_valid is not sf or sfc.")
    return(sf_object)
  }
  if (is_sf) {
    if (nrow(sf_object) == 0) return(sf_object)
    sf_object <- sf_object[!st_is_empty(sf_object$geometry), ]
    if (nrow(sf_object) == 0) return(sf_object)
    valid_sf <- tryCatch({
      st_make_valid(sf_object)
    }, error = function(e) {
      message("Warning: st_make_valid(sf) error: ", conditionMessage(e))
      return(sf_object)
    })
    if (inherits(valid_sf, "sf") && nrow(valid_sf) > 0) {
      valid_sf <- valid_sf[!st_is_empty(valid_sf$geometry), ]
    } else if (!inherits(valid_sf, "sf")) {
      message("Warning: st_make_valid(sf) invalid return.")
      return(sf_object)
    }
    return(valid_sf)
  }
  if (is_sfc) {
    if (length(sf_object) == 0) return(sf_object)
    sf_object <- sf_object[!st_is_empty(sf_object)]
    if (length(sf_object) == 0) return(sf_object)
    valid_sfc <- tryCatch({
      st_make_valid(sf_object)
    }, error = function(e) {
      message("Warning: st_make_valid(sfc) error: ", conditionMessage(e))
      return(sf_object)
    })
    if (inherits(valid_sfc, "sfc") && length(valid_sfc) > 0) {
      valid_sfc <- valid_sfc[!st_is_empty(valid_sfc)]
    } else if (!inherits(valid_sfc, "sfc")) {
      message("Warning: st_make_valid(sfc) invalid return.")
      return(sf_object)
    }
    return(valid_sfc)
  }
}

# --- Read Input Data ---
message("1. Reading input layer: ", input_file_path)
tryCatch(input_sf <- st_read(input_file_path), error = function(e) stop("Error reading input file: ", conditionMessage(e)))
if (!"Label" %in% names(input_sf)) {
  label_col_name <- names(input_sf)[toupper(names(input_sf))=="LABEL"]
  if(length(label_col_name)==1){
    message("    - Note: Renamed '", label_col_name,"' to 'Label'.")
    names(input_sf)[names(input_sf)==label_col_name]<-"Label"
  } else {
    stop("Missing 'Label' column.")
  }
}

# --- Read and Process Grid Boundary ---
message("Processing grid boundary file: ", grid_boundary_file_path)
raw_grid_tiles_sf <- NULL; global_dissolved_grid_sf <- NULL; valid_tiles_sf <- NULL
tryCatch({
  raw_grid_tiles_sf <- st_read(grid_boundary_file_path)
  global_dissolved_grid_sf <- raw_grid_tiles_sf %>% summarise(geometry = st_union(st_geometry(.))) %>% safe_make_valid()
  if(nrow(global_dissolved_grid_sf)==0 || all(st_is_empty(global_dissolved_grid_sf$geometry))) stop("Overall dissolved grid boundary invalid.")
  message("    - Overall dissolved grid boundary processed.")
  if (use_tile_specific_background) {
    if (!tile_id_column_name %in% names(raw_grid_tiles_sf)) stop(paste0("Tile ID column '", tile_id_column_name, "' not found."))
    valid_tiles_sf <- raw_grid_tiles_sf %>%
      filter(!is.na(.data[[tile_id_column_name]]) & trimws(as.character(.data[[tile_id_column_name]])) != "") %>%
      select(all_of(tile_id_column_name), geometry) %>% safe_make_valid()
    if (nrow(valid_tiles_sf) == 0) message(paste0("    - WARNING: No valid tiles found. Initial background (if tile-specific) might be empty.")) else message(paste0("    - Found ", nrow(valid_tiles_sf), " valid tiles."))
  } else message("    - Global background mode enabled for initial background source.")
}, error = function(e) stop("Error reading/processing grid boundary file: ", conditionMessage(e)))

# --- CRS Handling ---
message("Checking and harmonizing CRS...")
input_crs <- st_crs(input_sf)
if (is.na(input_crs)) {
  warning("Input lacks CRS. Trying grid's CRS.")
  if(!is.null(global_dissolved_grid_sf) && !is.na(st_crs(global_dissolved_grid_sf))){
    input_crs <- st_crs(global_dissolved_grid_sf)
    input_sf <- st_set_crs(input_sf, input_crs)
  } else stop("Both input and grid lack CRS.")
}
if (!is.null(global_dissolved_grid_sf)) {
  if (is.na(st_crs(global_dissolved_grid_sf))) global_dissolved_grid_sf <- st_set_crs(global_dissolved_grid_sf, input_crs)
  else if (st_crs(global_dissolved_grid_sf) != input_crs) {
    message("    - Reprojecting global grid...")
    global_dissolved_grid_sf <- st_transform(global_dissolved_grid_sf, input_crs)
  }
}
if (use_tile_specific_background && !is.null(valid_tiles_sf) && nrow(valid_tiles_sf) > 0) {
  if (is.na(st_crs(valid_tiles_sf))) valid_tiles_sf <- st_set_crs(valid_tiles_sf, input_crs)
  else if (st_crs(valid_tiles_sf) != input_crs) {
    message("    - Reprojecting valid tiles...")
    valid_tiles_sf <- st_transform(valid_tiles_sf, input_crs)
  }
}
target_crs_wkt <- st_crs(input_crs)$wkt; message("    - Target CRS:\n", target_crs_wkt)

# --- Preprocessing: Make input geometries valid ---
message("2. Checking and repairing input layer geometries...")
rows_before_preproc <- nrow(input_sf); input_sf <- safe_make_valid(input_sf); if(nrow(input_sf)<rows_before_preproc) message("    - ", rows_before_preproc-nrow(input_sf), " feature(s) removed."); if(nrow(input_sf)==0) stop("Input empty after repair.")


# --- Create Initial Background Layer ---
message("Creating initial background layer with label: '", background_label, "'")
background_initial_sf <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs) # Initialize

if (use_tile_specific_background) {
  if (!is.null(valid_tiles_sf) && nrow(valid_tiles_sf) > 0) {
    message("  - Using union of valid tiles for initial background.")
    bg_geom <- st_union(st_geometry(valid_tiles_sf)) %>% safe_make_valid() # valid_tiles_sf is already in input_crs
    if (length(bg_geom) > 0 && !all(st_is_empty(bg_geom))) {
      background_initial_sf <- st_sf(Label = background_label, geometry = bg_geom, crs = input_crs)
    } else {
      message("  - WARNING: Union of valid tiles for background resulted in empty geometry.")
    }
  } else {
    message("  - WARNING: Tile-specific background enabled, but NO valid tiles found. Initial background layer will be empty.")
  }
} else { # Use global background
  if (!is.null(global_dissolved_grid_sf) && nrow(global_dissolved_grid_sf) > 0 && !all(st_is_empty(global_dissolved_grid_sf$geometry))) {
    message("  - Using global dissolved grid for initial background.")
    # global_dissolved_grid_sf is already in input_crs
    background_initial_sf <- global_dissolved_grid_sf %>%
      mutate(Label = background_label) %>%
      select(Label, geometry) %>% # Ensure only Label and geometry are kept
      safe_make_valid()
  } else {
    message("  - WARNING: Global dissolved grid is empty or invalid. Initial background layer will be empty.")
  }
}
if(nrow(background_initial_sf) > 0) {
  message("  - Initial background layer created with ", nrow(background_initial_sf), " feature(s). Applying validation...")
  background_initial_sf <- safe_make_valid(background_initial_sf)
  if(nrow(background_initial_sf) > 0){
    message("    - Initial background layer valid with ", nrow(background_initial_sf), " feature(s).")
  } else {
    message("    - Initial background layer became empty after validation.")
  }
} else {
  message("  - Initial background layer is empty.")
}


# --- Step 3: Split, Repair, and Dissolve per label (excluding background label for now) ---
message("3. Splitting and dissolving data per label...")
dissolve_by_label <- function(data, label_value) {
  message("    - Processing label: ", label_value); target_crs<-st_crs(data); subset_sf <- data %>% filter(Label == label_value)
  if(nrow(subset_sf)==0){ message("      -> No features."); return(st_sf(Label=character(), geometry=st_sfc(crs=target_crs)))}
  subset_sf <- safe_make_valid(subset_sf)
  if(nrow(subset_sf)==0){ message("      -> No valid features after validation."); return(st_sf(Label=character(), geometry=st_sfc(crs=target_crs)))}
  
  # Attempt to buffer by 0 to fix potential self-intersections before union
  # subset_geom_buffered <- tryCatch(st_buffer(st_geometry(subset_sf), 0), error = function(e) {
  #    message("        -> Warning during pre-union buffer for '", label_value, "': ", e$message); st_geometry(subset_sf)
  # })
  # dissolved_geom <- tryCatch(st_union(subset_geom_buffered), error=function(e){ message("      -> Error st_union: ", e$message); st_sfc(crs=target_crs) })
  dissolved_geom <- tryCatch(st_union(st_geometry(subset_sf)), error=function(e){ message("      -> Error st_union: ", e$message); st_sfc(crs=target_crs) })
  
  if(!inherits(dissolved_geom, "sfc")){ message("      -> WARNING: st_union did not return sfc object."); return(st_sf(Label=character(), geometry=st_sfc(crs=target_crs)))}
  if(length(dissolved_geom)==0||all(st_is_empty(dissolved_geom))){ message("      -> Dissolve empty."); return(st_sf(Label=character(), geometry=st_sfc(crs=target_crs)))}
  if(is.na(st_crs(dissolved_geom))&&!is.na(target_crs)) st_crs(dissolved_geom)<-target_crs
  dissolved_sf <- st_sf(Label=label_value, geometry=dissolved_geom); dissolved_sf <- safe_make_valid(dissolved_sf)
  if(nrow(dissolved_sf)>0) message("      -> Dissolve completed.") else message("      -> Dissolve empty after final validation.")
  return(dissolved_sf)
}

inundated_dissolved <- dissolve_by_label(input_sf, "inundated"); if (!inherits(inundated_dissolved, "sf")) stop("dissolve failed for 'inundated'")
uncertain_dissolved <- dissolve_by_label(input_sf, "uncertain"); if (!inherits(uncertain_dissolved, "sf")) stop("dissolve failed for 'uncertain'")
other_dissolved     <- dissolve_by_label(input_sf, "other");     if (!inherits(other_dissolved, "sf")) stop("dissolve failed for 'other'")


# --- Step 4: Apply Prioritization (Difference) ---
# New Priority: inundated > other > uncertain > background_label
message("4. Applying prioritization... Priority: inundated > other > uncertain > '", background_label, "'")

# Initialize empty sf objects for processed layers
inundated_processed_sf <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
other_processed_sf     <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
uncertain_processed_sf <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
background_processed_sf<- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)

current_priority_union_geom <- st_sfc(crs = input_crs) # Accumulator for higher priority geometries

# 1. Process "inundated" (highest priority)
if (nrow(inundated_dissolved) > 0 && !all(st_is_empty(inundated_dissolved$geometry))) {
  inundated_processed_sf <- inundated_dissolved # Already has "Label" column and is validated
  if (nrow(inundated_processed_sf) > 0 && !all(st_is_empty(inundated_processed_sf$geometry))) {
    current_priority_union_geom <- st_union(st_geometry(inundated_processed_sf)) %>% safe_make_valid()
    message("    - 'inundated' (highest priority) processed: ", nrow(inundated_processed_sf), " features.")
  } else { # Should not happen if inundated_dissolved was valid and non-empty
    message("    - 'inundated' became empty unexpectedly during assignment for prioritization.")
    inundated_processed_sf <- st_sf(Label = "inundated", geometry = st_sfc(), crs = input_crs) # re-initialize to empty with label
  }
} else {
  message("    - 'inundated_dissolved' is empty or invalid, skipping.")
}

# 2. Process "other"
message("    - Calculating: 'other' minus processed 'inundated'")
if (nrow(other_dissolved) > 0 && !all(st_is_empty(other_dissolved$geometry))) {
  geom_to_diff <- st_geometry(other_dissolved)
  diff_geom_other <- geom_to_diff
  if (length(current_priority_union_geom) > 0 && !all(st_is_empty(current_priority_union_geom))) {
    diff_geom_other <- tryCatch(
      st_difference(geom_to_diff, current_priority_union_geom),
      error = function(e) { message("      -> Error in st_difference for 'other': ", e$message); st_sfc(crs = input_crs) }
    )
  }
  diff_geom_other <- diff_geom_other[st_geometry_type(diff_geom_other) %in% c("POLYGON", "MULTIPOLYGON")]
  if (length(diff_geom_other) > 0 && !all(st_is_empty(diff_geom_other))) {
    other_processed_sf <- st_sf(geometry = diff_geom_other, crs = input_crs) %>% mutate(Label = "other") %>% safe_make_valid()
    if (nrow(other_processed_sf) > 0 && !all(st_is_empty(other_processed_sf$geometry))) {
      message("      -> 'other' (prioritized) has ", nrow(other_processed_sf), " features.")
      new_addition_geom <- st_geometry(other_processed_sf)
      if (length(current_priority_union_geom) > 0 && !all(st_is_empty(current_priority_union_geom))) {
        current_priority_union_geom <- st_union(current_priority_union_geom, new_addition_geom) %>% safe_make_valid()
      } else {
        current_priority_union_geom <- st_union(new_addition_geom) %>% safe_make_valid()
      }
    } else {
      message("      -> 'other' (prioritized) became empty after validation.")
      other_processed_sf <- st_sf(Label = "other", geometry = st_sfc(), crs = input_crs)
    }
  } else {
    message("      -> 'other' (prioritized) is empty after difference or type filtering.")
  }
} else {
  message("    - 'other_dissolved' is empty or invalid, skipping.")
}

# 3. Process "uncertain"
message("    - Calculating: 'uncertain' minus processed ('inundated' + 'other')")
if (nrow(uncertain_dissolved) > 0 && !all(st_is_empty(uncertain_dissolved$geometry))) {
  geom_to_diff <- st_geometry(uncertain_dissolved)
  diff_geom_uncertain <- geom_to_diff
  if (length(current_priority_union_geom) > 0 && !all(st_is_empty(current_priority_union_geom))) {
    diff_geom_uncertain <- tryCatch(
      st_difference(geom_to_diff, current_priority_union_geom),
      error = function(e) { message("      -> Error in st_difference for 'uncertain': ", e$message); st_sfc(crs = input_crs) }
    )
  }
  diff_geom_uncertain <- diff_geom_uncertain[st_geometry_type(diff_geom_uncertain) %in% c("POLYGON", "MULTIPOLYGON")]
  if (length(diff_geom_uncertain) > 0 && !all(st_is_empty(diff_geom_uncertain))) {
    uncertain_processed_sf <- st_sf(geometry = diff_geom_uncertain, crs = input_crs) %>% mutate(Label = "uncertain") %>% safe_make_valid()
    if (nrow(uncertain_processed_sf) > 0 && !all(st_is_empty(uncertain_processed_sf$geometry))) {
      message("      -> 'uncertain' (prioritized) has ", nrow(uncertain_processed_sf), " features.")
      new_addition_geom <- st_geometry(uncertain_processed_sf)
      if (length(current_priority_union_geom) > 0 && !all(st_is_empty(current_priority_union_geom))) {
        current_priority_union_geom <- st_union(current_priority_union_geom, new_addition_geom) %>% safe_make_valid()
      } else {
        current_priority_union_geom <- st_union(new_addition_geom) %>% safe_make_valid()
      }
    } else {
      message("      -> 'uncertain' (prioritized) became empty after validation.")
      uncertain_processed_sf <- st_sf(Label = "uncertain", geometry = st_sfc(), crs = input_crs)
    }
  } else {
    message("      -> 'uncertain' (prioritized) is empty after difference or type filtering.")
  }
} else {
  message("    - 'uncertain_dissolved' is empty or invalid, skipping.")
}

# 4. Process "background_label" (lowest priority)
message("    - Calculating: '", background_label, "' minus processed ('inundated' + 'other' + 'uncertain')")
if (nrow(background_initial_sf) > 0 && !all(st_is_empty(background_initial_sf$geometry))) {
  geom_to_diff <- st_geometry(background_initial_sf)
  diff_geom_background <- geom_to_diff
  if (length(current_priority_union_geom) > 0 && !all(st_is_empty(current_priority_union_geom))) {
    diff_geom_background <- tryCatch(
      st_difference(geom_to_diff, current_priority_union_geom),
      error = function(e) { message("      -> Error in st_difference for background: ", e$message); st_sfc(crs = input_crs) }
    )
  }
  diff_geom_background <- diff_geom_background[st_geometry_type(diff_geom_background) %in% c("POLYGON", "MULTIPOLYGON")]
  if (length(diff_geom_background) > 0 && !all(st_is_empty(diff_geom_background))) {
    background_processed_sf <- st_sf(geometry = diff_geom_background, crs = input_crs) %>% mutate(Label = background_label) %>% safe_make_valid()
    if (nrow(background_processed_sf) > 0 && !all(st_is_empty(background_processed_sf$geometry))) {
      message("      -> '", background_label, "' (prioritized) has ", nrow(background_processed_sf), " features.")
    } else {
      message("      -> '", background_label, "' (prioritized) became empty after validation.")
      background_processed_sf <- st_sf(Label = background_label, geometry = st_sfc(), crs = input_crs)
    }
  } else {
    message("      -> '", background_label, "' (prioritized) is empty after difference or type filtering.")
  }
} else {
  message("    - 'background_initial_sf' is empty or invalid, skipping.")
}

# --- Step 5: Combine final prioritized layers ---
message("5. Combining final prioritized layers...")
final_layers_list <- list(inundated_processed_sf, other_processed_sf, uncertain_processed_sf, background_processed_sf)
valid_final_layers <- Filter(function(lyr) inherits(lyr, "sf") && nrow(lyr) > 0 && !all(st_is_empty(lyr$geometry)), final_layers_list)

# --- Main Logic: Process combined layers ---
final_object_for_output <- st_sf(Label=character(), geometry=st_sfc(crs=input_crs)) # Initialize

if (length(valid_final_layers) > 0) {
  message("  - Combining ", length(valid_final_layers), " valid prioritized layer(s) (including background)...")
  final_combined_sf <- do.call(rbind, valid_final_layers) %>%
    select(Label, geometry) %>%
    st_set_crs(input_crs) %>% # Explicitly set CRS
    safe_make_valid()
  
  if (nrow(final_combined_sf) > 0){
    message("6. Converting combined layers to singlepart polygons...")
    final_output_sf_singlepart <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs) # Initialize for this scope
    
    temp_processed_sf <- final_combined_sf
    if (any(st_geometry_type(temp_processed_sf) == "GEOMETRYCOLLECTION")) {
      message("    - Extracting POLYGONs from GEOMETRYCOLLECTIONs...")
      temp_processed_sf <- st_collection_extract(temp_processed_sf, type = "POLYGON") %>%
        filter(!st_is_empty(geometry), st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
    }
    
    if (nrow(temp_processed_sf) > 0) {
      message("    - Casting to POLYGON...")
      final_output_sf_singlepart <- st_cast(temp_processed_sf, "POLYGON") %>% safe_make_valid()
    }
    
    if (nrow(final_output_sf_singlepart) == 0) {
      message("    - All layers became empty after casting/final validation.")
      # final_object_for_output remains the initialized empty sf
    } else {
      message("    - Layers converted to singlepart: ", nrow(final_output_sf_singlepart), " features.")
      final_object_for_output <- final_output_sf_singlepart
    }
  } else {
    message("  - Combined layers are empty after initial rbind and validation.")
    # final_object_for_output remains the initialized empty sf
  }
} else { # No valid layers (input labels or initial background) found or all became empty
  message("No valid data layers found (input labels or initial background were empty or became invalid after prioritization). Output will be empty.")
  # final_object_for_output remains the initialized empty sf
}


# --- Step 7: Write Output ---
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
  message("Attempting to create output directory: ", output_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
}

if (exists("output_object") && inherits(output_object, "sf") && nrow(output_object) > 0 && dir.exists(output_dir)) {
  message("8. Writing final result (", nrow(output_object), " features) to: ", output_file_path)
  tryCatch({
    st_write(output_object, output_file_path, delete_layer = TRUE, quiet = FALSE)
    message("    - Write completed successfully.")
  }, error = function(e) warning("Error writing final output file: ", conditionMessage(e)))
} else if (!dir.exists(output_dir)){
  warning("Output directory could not be found or created: ", output_dir, ". Result NOT written.")
} else {
  message("8. Final result is empty or invalid after all processing steps, no output file written.")
}

message("--- Script Finished ---")
