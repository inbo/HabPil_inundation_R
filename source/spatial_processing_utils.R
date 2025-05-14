# --- Spatial Processing Utility Functions ---
# This file (e.g., source/spatial_processing_utils.R) contains functions for the core spatial analysis.
# It's good practice to use explicit namespace calls (e.g., sf::st_read) in utility scripts.

# --- Helper Function for safe geometry repair ---
safe_make_valid <- function(sf_object) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is needed for safe_make_valid.")
  
  is_sf <- inherits(sf_object, "sf")
  is_sfc <- inherits(sf_object, "sfc")
  if (!is_sf && !is_sfc) {
    message("Warning: Input for safe_make_valid is not sf or sfc.")
    return(sf_object)
  }
  if (is_sf) {
    if (nrow(sf_object) == 0) return(sf_object)
    # Correct way to get the geometry column name
    geom_col_name <- attr(sf_object, "sf_column") 
    if (is.null(geom_col_name) || !geom_col_name %in% names(sf_object)) {
      message("Warning: sf object in safe_make_valid does not have a recognized geometry column.")
      return(sf_object) 
    }
    if (nrow(sf_object) > 0) {
      sf_object <- sf_object[!sf::st_is_empty(sf_object[[geom_col_name]]), ]
    }
    if (nrow(sf_object) == 0) return(sf_object)
    
    valid_sf <- tryCatch({
      sf::st_make_valid(sf_object) 
    }, error = function(e) {
      message("Warning: st_make_valid(sf) error: ", conditionMessage(e))
      return(sf_object)
    })
    if (inherits(valid_sf, "sf") && nrow(valid_sf) > 0) {
      # Correct way to get the geometry column name for the validated object
      geom_col_name_valid <- attr(valid_sf, "sf_column") 
      if (is.null(geom_col_name_valid) || !geom_col_name_valid %in% names(valid_sf)) {
        message("Warning: Validated sf object in safe_make_valid does not have a recognized geometry column.")
        return(sf_object) 
      }
      if (nrow(valid_sf) > 0) { 
        valid_sf <- valid_sf[!sf::st_is_empty(valid_sf[[geom_col_name_valid]]), ]
      }
    } else if (!inherits(valid_sf, "sf")) {
      message("Warning: st_make_valid(sf) invalid return.")
      return(sf_object)
    }
    return(valid_sf)
  }
  if (is_sfc) {
    if (length(sf_object) == 0) return(sf_object)
    sf_object <- sf_object[!sf::st_is_empty(sf_object)] 
    if (length(sf_object) == 0) return(sf_object)
    valid_sfc <- tryCatch({
      sf::st_make_valid(sf_object) 
    }, error = function(e) {
      message("Warning: st_make_valid(sfc) error: ", conditionMessage(e))
      return(sf_object)
    })
    if (inherits(valid_sfc, "sfc") && length(valid_sfc) > 0) {
      valid_sfc <- valid_sfc[!sf::st_is_empty(valid_sfc)] 
    } else if (!inherits(valid_sfc, "sfc")) {
      message("Warning: st_make_valid(sfc) invalid return.")
      return(sf_object)
    }
    return(valid_sfc)
  }
}

# --- Helper Function to Dissolve by Label ---
dissolve_by_label <- function(data, label_value, target_crs_obj) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is needed for dissolve_by_label.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is needed for dissolve_by_label.")
  
  message("     - Processing label: ", label_value)
  subset_sf <- dplyr::filter(data, Label == label_value)
  
  if(nrow(subset_sf) == 0){ 
    message("       -> No features for this label."); 
    return(sf::st_sf(data.frame(Label = character(0)), geometry = sf::st_sfc(crs = target_crs_obj), crs = target_crs_obj)) 
  }
  subset_sf <- safe_make_valid(subset_sf) 
  if(nrow(subset_sf) == 0){ 
    message("       -> No valid features after validation for this label."); 
    return(sf::st_sf(data.frame(Label = character(0)), geometry = sf::st_sfc(crs = target_crs_obj), crs = target_crs_obj))
  }
  
  dissolved_geom <- tryCatch(
    sf::st_union(sf::st_geometry(subset_sf)), 
    error = function(e){ 
      message("       -> Error during st_union for '", label_value, "': ", e$message); 
      sf::st_sfc(crs = target_crs_obj) 
    })
  
  if(!inherits(dissolved_geom, "sfc")){ 
    message("       -> WARNING: st_union for '", label_value, "' did not return sfc object."); 
    return(sf::st_sf(data.frame(Label = character(0)), geometry = sf::st_sfc(crs = target_crs_obj), crs = target_crs_obj))
  }
  if(length(dissolved_geom) == 0 || all(sf::st_is_empty(dissolved_geom))){ 
    message("       -> Dissolve for '", label_value, "' resulted in empty geometry."); 
    return(sf::st_sf(data.frame(Label = character(0)), geometry = sf::st_sfc(crs = target_crs_obj), crs = target_crs_obj))
  }
  if(is.na(sf::st_crs(dissolved_geom)) && !is.na(target_crs_obj)) { 
    sf::st_crs(dissolved_geom) <- target_crs_obj 
  }
  
  dissolved_sf <- sf::st_sf(data.frame(Label = label_value), geometry = dissolved_geom, crs = target_crs_obj) 
  dissolved_sf <- safe_make_valid(dissolved_sf)
  
  if(nrow(dissolved_sf) > 0) {
    message("       -> Dissolve for '", label_value, "' completed with ", nrow(dissolved_sf), " feature(s).") 
  } else {
    message("       -> Dissolve for '", label_value, "' resulted in empty geometry after final validation.")
    dissolved_sf <- sf::st_sf(data.frame(Label = character(0)), geometry = sf::st_sfc(crs = target_crs_obj), crs = target_crs_obj)
  }
  return(dissolved_sf)
}

# --- Sub-function: Read and Prepare Input Labels ---
read_and_prepare_labels_sf <- function(labels_path) {
  message("Sub-function: Reading and preparing input labels from: ", labels_path)
  tryCatch(
    input_sf <- sf::st_read(labels_path, quiet = TRUE), 
    error = function(e) stop("Error reading input file (labels): ", conditionMessage(e))
  )
  if (!"Label" %in% names(input_sf)) {
    label_col_name_detected <- names(input_sf)[toupper(names(input_sf))=="LABEL"]
    if(length(label_col_name_detected)==1){
      message("     - Note: Renamed column '", label_col_name_detected,"' to 'Label'.")
      names(input_sf)[names(input_sf)==label_col_name_detected]<-"Label"
    } else {
      stop("Missing 'Label' column in input labels file.")
    }
  }
  message("     - Input labels read successfully: ", nrow(input_sf), " features.")
  return(input_sf)
}

# --- Sub-function: Read and Prepare Grid Tiles ---
read_and_prepare_grid_sf <- function(grid_path, tile_id_col_name_config) {
  message("Sub-function: Reading and preparing grid tiles from: ", grid_path)
  raw_grid_tiles_sf <- NULL
  valid_tiles_sf <- NULL
  grid_crs_obj <- NULL
  
  tryCatch({
    raw_grid_tiles_sf <- sf::st_read(grid_path, quiet = TRUE) 
    if (!inherits(raw_grid_tiles_sf, "sf")) {
      stop("Failed to read grid boundary file as an sf object: ", grid_path)
    }
    grid_crs_obj <- sf::st_crs(raw_grid_tiles_sf) 
    
    geom_col_grid <- attr(raw_grid_tiles_sf, "sf_column")
    if(is.null(geom_col_grid) || !geom_col_grid %in% names(raw_grid_tiles_sf)){
      stop("Could not identify geometry column in grid tiles file.")
    }
    if(geom_col_grid != "geometry") { 
      message("     - Renaming geometry column in grid tiles from '", geom_col_grid, "' to 'geometry'.")
      raw_grid_tiles_sf <- dplyr::rename(raw_grid_tiles_sf, geometry = dplyr::all_of(geom_col_grid))
      sf::st_geometry(raw_grid_tiles_sf) <- raw_grid_tiles_sf$geometry 
    }
    
    if (!tile_id_col_name_config %in% names(raw_grid_tiles_sf)) {
      stop(paste0("Tile ID column '", tile_id_col_name_config, "' not found in grid tiles file."))
    }
    
    message("     - Filtering and selecting columns from raw grid tiles...")
    temp_filtered_tiles <- raw_grid_tiles_sf %>%
      dplyr::filter(!is.na(.data[[tile_id_col_name_config]]) & trimws(as.character(.data[[tile_id_col_name_config]])) != "") %>%
      dplyr::select(dplyr::all_of(tile_id_col_name_config), geometry) 
    
    if (!inherits(temp_filtered_tiles, "sf")) {
      stop("Filtering grid tiles did not result in an sf object.")
    }
    message("     - Applying safe_make_valid to ", nrow(temp_filtered_tiles), " filtered tiles...")
    
    if (!is.function(safe_make_valid)) { 
      stop("'safe_make_valid' is not defined as a function. Check script sourcing.")
    }
    valid_tiles_sf <- safe_make_valid(temp_filtered_tiles)
    
    if (nrow(valid_tiles_sf) == 0) {
      message(paste0("     - WARNING: No valid tiles found after filtering by '", tile_id_col_name_config, 
                     "' and validation. The grid cannot be used for background creation if it remains empty."))
    } else {
      message(paste0("     - Found ", nrow(valid_tiles_sf), " valid individual tiles."))
    }
  }, error = function(e) {
    detailed_error_message <- paste("Error reading/processing grid boundary file '", grid_path, "': ", conditionMessage(e), sep="")
    if(exists("raw_grid_tiles_sf") && !is.null(raw_grid_tiles_sf)) message("Debug: class(raw_grid_tiles_sf) = ", class(raw_grid_tiles_sf))
    if(exists("temp_filtered_tiles") && !is.null(temp_filtered_tiles)) message("Debug: class(temp_filtered_tiles) = ", class(temp_filtered_tiles))
    stop(detailed_error_message)
  })
  return(list(valid_tiles = valid_tiles_sf, grid_crs = grid_crs_obj))
}

# --- Sub-function: Harmonize CRS ---
harmonize_crs_all <- function(input_labels, grid_tiles_valid, original_grid_crs) {
  message("Sub-function: Harmonizing CRS...")
  
  current_input_crs <- sf::st_crs(input_labels)
  target_crs <- current_input_crs 
  
  if (is.na(current_input_crs)) {
    warning("Input labels file lacks CRS. Attempting to use grid's original CRS.")
    if(!is.null(original_grid_crs) && !is.na(original_grid_crs)){ 
      target_crs <- original_grid_crs
      input_labels <- sf::st_set_crs(input_labels, target_crs) 
      message("     - Input labels CRS set using grid's original CRS.")
    } else {
      stop("Input labels file lacks CRS, and grid file also lacks CRS or failed to load. Cannot proceed.")
    }
  }
  
  if (!is.null(grid_tiles_valid) && nrow(grid_tiles_valid) > 0) {
    if (is.na(sf::st_crs(grid_tiles_valid))) { 
      grid_tiles_valid <- sf::st_set_crs(grid_tiles_valid, target_crs) 
    } else if (sf::st_crs(grid_tiles_valid) != target_crs) { 
      message("     - Reprojecting valid tiles to target CRS...")
      grid_tiles_valid <- sf::st_transform(grid_tiles_valid, target_crs) 
    }
  }
  
  if (sf::st_crs(input_labels) != target_crs) {
    message("     - Reprojecting input labels to target CRS...")
    input_labels <- sf::st_transform(input_labels, target_crs)
  }
  
  target_crs_wkt <- sf::st_crs(target_crs)$wkt 
  message("     - Target CRS for all layers:\n", target_crs_wkt)
  
  return(list(input_sf_harmonized = input_labels, 
              valid_tiles_harmonized = grid_tiles_valid, 
              target_crs = target_crs))
}

# --- Sub-function: Create Initial Background ---
create_initial_background_sf <- function(grid_tiles_for_background, target_crs_obj, background_label_config) {
  message("Sub-function: Creating initial background layer with label: '", background_label_config, "'")
  
  initial_background <- sf::st_sf(data.frame(Label = character(0)), 
                                  geometry = sf::st_sfc(crs = target_crs_obj), 
                                  crs = target_crs_obj)
  
  if (!is.null(grid_tiles_for_background) && nrow(grid_tiles_for_background) > 0 && !all(sf::st_is_empty(grid_tiles_for_background$geometry))) { 
    message("   - Using union of processed valid tiles for initial background.")
    bg_geom <- sf::st_union(sf::st_geometry(grid_tiles_for_background)) %>% safe_make_valid() 
    if (length(bg_geom) > 0 && !all(sf::st_is_empty(bg_geom))) { 
      initial_background <- sf::st_sf(data.frame(Label = background_label_config), 
                                      geometry = bg_geom, 
                                      crs = target_crs_obj) 
    } else {
      message("   - WARNING: Union of valid tiles for background resulted in empty or invalid geometry.")
    }
  } else {
    message("   - WARNING: No valid tiles provided or tiles are empty. Initial background layer will be empty.")
  }
  
  if(nrow(initial_background) > 0) {
    message("   - Initial background layer created with ", nrow(initial_background), " feature(s). Applying final validation...")
    initial_background <- safe_make_valid(initial_background) 
    if(nrow(initial_background) > 0){
      message("     - Initial background layer is valid with ", nrow(initial_background), " feature(s).")
    } else {
      message("     - WARNING: Initial background layer became empty after final validation.")
      initial_background <- sf::st_sf(data.frame(Label = character(0)), 
                                      geometry = sf::st_sfc(crs = target_crs_obj), 
                                      crs = target_crs_obj)
    }
  } else {
    message("   - Initial background layer is empty.")
  }
  return(initial_background)
}

# --- Sub-function: Apply Layer Prioritization ---
apply_layer_prioritization_sf <- function(in_dissolved, ot_dissolved, un_dissolved, bg_initial, 
                                          bg_label_config, target_crs_obj) {
  message("Sub-function: Applying layer prioritization...")
  
  inundated_proc <- sf::st_sf(data.frame(Label=character(0)), geometry=sf::st_sfc(crs=target_crs_obj), crs=target_crs_obj)
  other_proc     <- sf::st_sf(data.frame(Label=character(0)), geometry=sf::st_sfc(crs=target_crs_obj), crs=target_crs_obj)
  uncertain_proc <- sf::st_sf(data.frame(Label=character(0)), geometry=sf::st_sfc(crs=target_crs_obj), crs=target_crs_obj)
  background_proc<- sf::st_sf(data.frame(Label=character(0)), geometry=sf::st_sfc(crs=target_crs_obj), crs=target_crs_obj)
  
  current_priority_union <- sf::st_sfc(crs = target_crs_obj) 
  
  # 1. Process "inundated"
  if (nrow(in_dissolved) > 0 && !all(sf::st_is_empty(in_dissolved$geometry))) { 
    inundated_proc <- in_dissolved 
    current_priority_union <- sf::st_union(sf::st_geometry(inundated_proc)) %>% safe_make_valid() 
    message("     - 'inundated' (highest priority) processed: ", nrow(inundated_proc), " features.")
  } else {
    message("     - 'inundated_dissolved' is empty. 'inundated_proc' remains a 0-row sf object.")
  }
  
  # 2. Process "other"
  message("     - Calculating: 'other' minus processed 'inundated'")
  if (nrow(ot_dissolved) > 0 && !all(sf::st_is_empty(ot_dissolved$geometry))) { 
    geom_to_diff <- sf::st_geometry(ot_dissolved) 
    diff_geom <- geom_to_diff 
    if (length(current_priority_union) > 0 && !all(sf::st_is_empty(current_priority_union))) { 
      diff_geom <- tryCatch(sf::st_difference(geom_to_diff, current_priority_union), 
                            error = function(e) { message("       -> Error in st_difference for 'other': ", e$message); sf::st_sfc(crs = target_crs_obj) } 
      )
    }
    diff_geom <- diff_geom[sf::st_geometry_type(diff_geom) %in% c("POLYGON", "MULTIPOLYGON")] 
    if (length(diff_geom) > 0 && !all(sf::st_is_empty(diff_geom))) { 
      temp_sf <- sf::st_sf(data.frame(Label = "other"), geometry = diff_geom, crs = target_crs_obj) %>% safe_make_valid() 
      if (nrow(temp_sf) > 0 && !all(sf::st_is_empty(temp_sf$geometry))) { 
        other_proc <- temp_sf
        message("       -> 'other' (prioritized) has ", nrow(other_proc), " features.")
        current_priority_union <- sf::st_union(current_priority_union, sf::st_geometry(other_proc)) %>% safe_make_valid() 
      } else { message("       -> 'other' (prioritized) became empty after validation.") }
    } else { message("       -> 'other' (prioritized) is empty after difference or type filtering.") }
  } else { message("     - 'other_dissolved' is empty. 'other_proc' remains a 0-row sf object.") }
  
  # 3. Process "uncertain"
  message("     - Calculating: 'uncertain' minus processed ('inundated' + 'other')")
  if (nrow(un_dissolved) > 0 && !all(sf::st_is_empty(un_dissolved$geometry))) { 
    geom_to_diff <- sf::st_geometry(un_dissolved) 
    diff_geom <- geom_to_diff
    if (length(current_priority_union) > 0 && !all(sf::st_is_empty(current_priority_union))) { 
      diff_geom <- tryCatch(sf::st_difference(geom_to_diff, current_priority_union),
                            error = function(e) { message("       -> Error in st_difference for 'uncertain': ", e$message); sf::st_sfc(crs = target_crs_obj) }
      )
    }
    diff_geom <- diff_geom[sf::st_geometry_type(diff_geom) %in% c("POLYGON", "MULTIPOLYGON")] 
    if (length(diff_geom) > 0 && !all(sf::st_is_empty(diff_geom))) { 
      temp_sf <- sf::st_sf(data.frame(Label = "uncertain"), geometry = diff_geom, crs = target_crs_obj) %>% safe_make_valid() 
      if (nrow(temp_sf) > 0 && !all(sf::st_is_empty(temp_sf$geometry))) { 
        uncertain_proc <- temp_sf
        message("       -> 'uncertain' (prioritized) has ", nrow(uncertain_proc), " features.")
        current_priority_union <- sf::st_union(current_priority_union, sf::st_geometry(uncertain_proc)) %>% safe_make_valid() 
      } else { message("       -> 'uncertain' (prioritized) became empty after validation.") }
    } else { message("       -> 'uncertain' (prioritized) is empty after difference or type filtering.") }
  } else { message("     - 'uncertain_dissolved' is empty. 'uncertain_proc' remains a 0-row sf object.") }
  
  # 4. Process "background_label"
  message("     - Calculating: '", bg_label_config, "' minus processed ('inundated' + 'other' + 'uncertain')")
  if (nrow(bg_initial) > 0 && !all(sf::st_is_empty(bg_initial$geometry))) { 
    geom_to_diff <- sf::st_geometry(bg_initial) 
    diff_geom <- geom_to_diff
    if (length(current_priority_union) > 0 && !all(sf::st_is_empty(current_priority_union))) { 
      diff_geom <- tryCatch(sf::st_difference(geom_to_diff, current_priority_union),
                            error = function(e) { message("       -> Error in st_difference for background: ", e$message); sf::st_sfc(crs = target_crs_obj) }
      )
    }
    diff_geom <- diff_geom[sf::st_geometry_type(diff_geom) %in% c("POLYGON", "MULTIPOLYGON")] 
    if (length(diff_geom) > 0 && !all(sf::st_is_empty(diff_geom))) { 
      temp_sf <- sf::st_sf(data.frame(Label = bg_label_config), geometry = diff_geom, crs = target_crs_obj) %>% safe_make_valid() 
      if (nrow(temp_sf) > 0 && !all(sf::st_is_empty(temp_sf$geometry))) { 
        background_proc <- temp_sf
        message("       -> '", bg_label_config, "' (prioritized) has ", nrow(background_proc), " features.")
      } else { message("       -> '", bg_label_config, "' (prioritized) became empty after validation.") }
    } else { message("       -> '", bg_label_config, "' (prioritized) is empty after difference or type filtering.") }
  } else { message("     - 'background_initial_sf' is empty. 'background_proc' remains a 0-row sf object.") }
  
  return(list(
    inundated = inundated_proc,
    other = other_proc,
    uncertain = uncertain_proc,
    background = background_proc
  ))
}

# --- Sub-function: Combine and Finalize Output ---
combine_and_finalize_layers_sf <- function(prioritized_list, target_crs_obj) {
  message("Sub-function: Combining and finalizing layers...")
  
  valid_final_layers <- Filter(function(lyr) {
    inherits(lyr, "sf") && 
      "Label" %in% names(lyr) && 
      inherits(sf::st_geometry(lyr), "sfc") && 
      nrow(lyr) > 0 && 
      !all(sf::st_is_empty(lyr$geometry)) 
  }, prioritized_list)
  
  final_output <- sf::st_sf(data.frame(Label=character(0)), geometry=sf::st_sfc(crs=target_crs_obj), crs=target_crs_obj)
  
  if (length(valid_final_layers) > 0) {
    message("   - Combining ", length(valid_final_layers), " valid prioritized layer(s)...")
    
    final_combined <- do.call(rbind, valid_final_layers)
    final_combined <- sf::st_set_crs(final_combined, target_crs_obj) %>% safe_make_valid()
    
    if (nrow(final_combined) > 0){
      message("   - Converting combined layers to singlepart polygons...")
      temp_processed <- final_combined
      if (any(sf::st_is(temp_processed, "GEOMETRYCOLLECTION"))) { 
        message("     - Extracting POLYGONs from GEOMETRYCOLLECTIONs...")
        temp_processed <- sf::st_collection_extract(temp_processed, type = "POLYGON") 
        temp_processed <- temp_processed %>%
          dplyr::filter(!sf::st_is_empty(geometry), sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON"))) %>% 
          safe_make_valid() 
      }
      
      if (nrow(temp_processed) > 0) {
        message("     - Casting to POLYGON (singlepart)...")
        if (any(sf::st_is(temp_processed, "MULTIPOLYGON"))) { 
          final_output <- sf::st_cast(temp_processed, "POLYGON") %>% safe_make_valid() 
        } else if (all(sf::st_is(temp_processed, "POLYGON"))) { 
          final_output <- temp_processed %>% safe_make_valid() 
        } else {
          message("     - WARNING: Not all geometries are MULTIPOLYGON or POLYGON after collection_extract. Attempting to filter for POLYGONs.")
          poly_only <- dplyr::filter(temp_processed, sf::st_is(geometry, "POLYGON")) 
          if(nrow(poly_only) > 0) {
            final_output <- safe_make_valid(poly_only)
          } else {
            message("     - No POLYGON geometries found after attempting to filter non-MULTIPOLYGON types.")
          }
        }
      }
      
      if (nrow(final_output) == 0) {
        message("     - All layers became empty after casting to singlepart polygons or final validation.")
      } else {
        message("     - Layers converted to singlepart polygons: ", nrow(final_output), " features.")
      }
    } else {
      message("   - Combined layers are empty after initial rbind and validation.")
    }
  } else { 
    message("No valid data layers found to combine. Output will be empty.")
  }
  return(final_output)
}


# --- Main Spatial Processing Function (Orchestrator) ---
process_spatial_layers <- function(input_labels_path, 
                                   input_grid_path, 
                                   config_params) { 
  
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is needed for process_spatial_layers.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is needed for process_spatial_layers.")
  
  background_label_conf <- config_params$background_label
  tile_id_column_name_conf <- config_params$tile_id_column_name
  
  message("\n--- Starting Spatial Data Processing Workflow (Refactored) ---")
  
  # Step 1: Read and Prepare Input Labels
  input_sf_prepared <- read_and_prepare_labels_sf(input_labels_path)
  if(nrow(input_sf_prepared) == 0) stop("Input labels layer is empty after initial read and preparation.")
  
  # Step 2: Read and Prepare Grid Tiles
  grid_data <- read_and_prepare_grid_sf(input_grid_path, tile_id_column_name_conf)
  valid_tiles_prepared <- grid_data$valid_tiles
  original_grid_crs_obj <- grid_data$grid_crs
  # valid_tiles_prepared can be NULL or 0-row if grid processing fails or yields no valid tiles
  
  # Step 3: Harmonize CRS
  crs_data <- harmonize_crs_all(input_sf_prepared, valid_tiles_prepared, original_grid_crs_obj)
  input_sf_harmonized <- crs_data$input_sf_harmonized
  valid_tiles_harmonized <- crs_data$valid_tiles_harmonized
  target_crs_obj <- crs_data$target_crs
  if(nrow(input_sf_harmonized) == 0) stop("Input labels layer became empty after CRS harmonization.")
  
  
  # Step 4: Pre-validate input_sf_harmonized before dissolving
  message("Pre-validating harmonized input labels before dissolving...")
  input_sf_harmonized <- safe_make_valid(input_sf_harmonized)
  if(nrow(input_sf_harmonized) == 0) {
    stop("Input labels layer is empty after pre-dissolve validation. Cannot proceed.")
  }
  
  # Step 5: Create Initial Background Layer
  background_initial_created <- create_initial_background_sf(valid_tiles_harmonized, target_crs_obj, background_label_conf)
  # background_initial_created can be a 0-row sf object
  
  # Step 6: Splitting and dissolving data per label
  message("Splitting and dissolving harmonized input data by label...")
  inundated_dissolved_processed <- dissolve_by_label(input_sf_harmonized, "inundated", target_crs_obj)
  uncertain_dissolved_processed <- dissolve_by_label(input_sf_harmonized, "uncertain", target_crs_obj)
  other_dissolved_processed     <- dissolve_by_label(input_sf_harmonized, "other", target_crs_obj)
  
  # Step 7: Apply Layer Prioritization
  message("Applying layer prioritization...")
  prioritized_layers <- apply_layer_prioritization_sf(
    in_dissolved = inundated_dissolved_processed,
    ot_dissolved = other_dissolved_processed,
    un_dissolved = uncertain_dissolved_processed,
    bg_initial = background_initial_created,
    bg_label_config = background_label_conf,
    target_crs_obj = target_crs_obj
  )
  
  # Step 8: Combine and Finalize Output
  message("Combining and finalizing output layers...")
  final_output_object <- combine_and_finalize_layers_sf(
    prioritized_list = prioritized_layers, # This is now a list of sf objects
    target_crs_obj = target_crs_obj
  )
  
  message("--- Spatial Data Processing Workflow Finished ---")
  return(final_output_object)
}

sample_with_min_dist <- function(polygons, n_target, min_dist, oversample_factor = 10) {
  n_target <- max(0, floor(n_target))
  if (n_target == 0) { message(" -> Target is 0 points."); return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(polygons)))) }
  if (!inherits(polygons, "sf")) { polygons <- sf::st_sf(geometry = polygons) }
  n_candidates <- max(ceiling(n_target * oversample_factor), 100)
  message(paste(" -> Generating", n_candidates, "candidate points..."))
  candidate_points_sfc <- sf::st_sample(polygons, size = n_candidates, type = "random", exact = TRUE)
  if (length(candidate_points_sfc) == 0) { warning("Could not generate any candidate points."); return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(polygons)))) }
  n_generated_candidates <- length(candidate_points_sfc); message(paste(" ->", n_generated_candidates, "candidates generated."))
  if (n_target == 1 || n_generated_candidates == 1) { message(" -> Selecting the first candidate point."); return(sf::st_sf(geometry = candidate_points_sfc[1])) }
  candidate_points_sf <- sf::st_sf(geometry = candidate_points_sfc)
  shuffled_indices <- sample(1:n_generated_candidates); selected_indices <- c(shuffled_indices[1])
  message(" -> Starting thinning process to ensure minimum distance...")
  pb <- txtProgressBar(min = 0, max = length(shuffled_indices), style = 3)
  for (i in 2:length(shuffled_indices)) {
    setTxtProgressBar(pb, i); current_candidate_index <- shuffled_indices[i]
    selected_points_geom <- candidate_points_sf$geometry[selected_indices]; current_point_geom <- candidate_points_sf$geometry[current_candidate_index]
    distances <- sf::st_distance(current_point_geom, selected_points_geom)
    tolerance <- units::set_units(1e-6, "m")
    if (min(distances) >= (min_dist - tolerance)) { selected_indices <- c(selected_indices, current_candidate_index) }
    if (length(selected_indices) >= n_target) { setTxtProgressBar(pb, length(shuffled_indices)); break }
  }
  close(pb); final_points_sf <- candidate_points_sf[selected_indices, ]
  n_final <- nrow(final_points_sf); message(paste(" ->", n_final, "points selected."))
  if (n_final < n_target) { warning(paste("Could only generate", n_final, "of target", n_target, "points respecting", min_dist, "distance")) }
  return(final_points_sf)
}

message("Spatial processing utility functions defined (safe_make_valid, dissolve_by_label, sample_with_min_dist and new sub-functions).")
