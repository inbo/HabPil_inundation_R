# Load required R packages
library(sf)
library(dplyr)

# --- Configuration ---
# --- Input and Output Files ---
input_file_path <- "E:/2025_BiodiversaHabPilot/Inundation/Labeled LabelMe/Afbakening extra Webbekoms Broek buiten SBZ/2023/Conversion to shapefile/polygons_py.shp"
output_file_path <- "output/labeled/Afbakening extra Webbekoms Broek buiten SBZ/2023/Web_Broek_2023_final_labeled.gpkg"

# --- Grid Boundary File & Tile ID ---
grid_boundary_file_path <- "data/Tiles LabelMe/Afbakening extra Webbekoms Broek buiten SBZ/Tiles_sel.shp"
tile_id_column_name <- "TileID"  # Confirmed by user

# --- Behavior Flags ---
use_tile_specific_background <- TRUE # TRUE: Fill background ONLY in specified valid tiles; FALSE: Use global dissolved grid boundary for background
background_label <- "not inundated"
remove_slivers <- TRUE
area_threshold <- 1.0  # Minimum area in square units of the CRS for sliver removal

# --- Function for safe geometry repair ---
safe_make_valid <- function(sf_object) {
  if (!inherits(sf_object, "sf") && !inherits(sf_object, "sfc")) {
    message("Warning: Input for safe_make_valid is not an sf or sfc object.")
    return(sf_object)
  }
  if (inherits(sf_object, "sf")) {
    if (nrow(sf_object) == 0) return(sf_object)
    sf_object <- sf_object[!st_is_empty(sf_object$geometry), ]
    if (nrow(sf_object) == 0) return(sf_object)
    valid_sf <- tryCatch(st_make_valid(sf_object), error = function(e) {
      message("Warning: st_make_valid (sf) error: ", e$message); sf_object })
    if(inherits(valid_sf, "sf") && nrow(valid_sf) > 0) valid_sf <- valid_sf[!st_is_empty(valid_sf$geometry), ]
    return(valid_sf)
  }
  if (inherits(sf_object, "sfc")) {
    if (length(sf_object) == 0) return(sf_object)
    sf_object <- sf_object[!st_is_empty(sf_object)]
    if (length(sf_object) == 0) return(sf_object)
    valid_sfc <- tryCatch(st_make_valid(sf_object), error = function(e) {
      message("Warning: st_make_valid (sfc) error: ", e$message); sf_object })
    if(inherits(valid_sfc, "sfc") && length(valid_sfc) > 0) valid_sfc <- valid_sfc[!st_is_empty(valid_sfc)]
    return(valid_sfc)
  }
}

# --- Read Input Data ---
message("1. Reading input layer: ", input_file_path)
tryCatch(input_sf <- st_read(input_file_path), error = function(e) stop("Error reading input file: ", e$message))

if (!"Label" %in% names(input_sf)) {
  label_col_name <- names(input_sf)[toupper(names(input_sf)) == "LABEL"]
  if (length(label_col_name) == 1) {
    message("   - Note: Column '", label_col_name, "' (case-insensitive) renamed to 'Label'.")
    names(input_sf)[names(input_sf) == label_col_name] <- "Label"
  } else stop("Input layer missing required 'Label' column.")
}

# --- Read and Process Grid Boundary ---
message("Processing grid boundary file: ", grid_boundary_file_path)
raw_grid_tiles_sf <- NULL
global_dissolved_grid_sf <- NULL # For fallback if use_tile_specific_background is FALSE
valid_tiles_sf <- NULL           # For tile-specific background if enabled and tiles are valid

tryCatch({
  raw_grid_tiles_sf <- st_read(grid_boundary_file_path)
  
  global_dissolved_grid_sf <- raw_grid_tiles_sf %>%
    summarise(geometry = st_union(st_geometry(.))) %>%
    safe_make_valid()
  if(nrow(global_dissolved_grid_sf) == 0 || all(st_is_empty(global_dissolved_grid_sf$geometry))) {
    stop("Overall dissolved grid boundary is empty or invalid. This is needed even if tile-specific background is false.")
  }
  message("   - Overall dissolved grid boundary processed (used if 'use_tile_specific_background' is FALSE or as a general extent).")
  
  if (use_tile_specific_background) {
    if (!tile_id_column_name %in% names(raw_grid_tiles_sf)) {
      stop(paste0("Specified Tile ID column '", tile_id_column_name, "' not found in grid file. Check name/case."))
    }
    valid_tiles_sf <- raw_grid_tiles_sf %>%
      filter(!is.na(.data[[tile_id_column_name]]) & trimws(as.character(.data[[tile_id_column_name]])) != "") %>%
      select(all_of(tile_id_column_name), geometry) %>%
      safe_make_valid()
    
    if (nrow(valid_tiles_sf) == 0) {
      message(paste0("   - WARNING: No grid tiles found with a non-empty '", tile_id_column_name,
                     "'. Since 'use_tile_specific_background' is TRUE, NO background will be generated."))
    } else {
      message(paste0("   - Found ", nrow(valid_tiles_sf), " tiles with non-empty '",
                     tile_id_column_name, "' for tile-specific background processing."))
    }
  } else {
    message("   - 'use_tile_specific_background' is FALSE. Global dissolved grid boundary will be used for background if needed.")
  }
}, error = function(e) {
  stop("Error reading/processing grid boundary file: ", e$message)
})

# --- CRS Handling ---
message("Checking and harmonizing Coordinate Reference Systems (CRS)...")
input_crs <- st_crs(input_sf)
if (is.na(input_crs)) {
  warning("Input layer has no defined CRS. Attempting to use CRS from global grid boundary.")
  if(!is.na(st_crs(global_dissolved_grid_sf))){
    input_crs <- st_crs(global_dissolved_grid_sf)
    input_sf <- st_set_crs(input_sf, input_crs)
  } else {
    stop("Both input layer and global grid boundary lack a defined CRS. Cannot proceed.")
  }
}

# Conform global_dissolved_grid_sf to input_crs
if (is.na(st_crs(global_dissolved_grid_sf))) {
  global_dissolved_grid_sf <- st_set_crs(global_dissolved_grid_sf, input_crs)
} else if (st_crs(global_dissolved_grid_sf) != input_crs) {
  message("   - Reprojecting global dissolved grid boundary to match input layer CRS.")
  global_dissolved_grid_sf <- st_transform(global_dissolved_grid_sf, input_crs)
}

# Conform valid_tiles_sf (if used and exists) to input_crs
if (use_tile_specific_background && !is.null(valid_tiles_sf) && nrow(valid_tiles_sf) > 0) {
  if (is.na(st_crs(valid_tiles_sf))) {
    valid_tiles_sf <- st_set_crs(valid_tiles_sf, input_crs)
  } else if (st_crs(valid_tiles_sf) != input_crs) {
    message("   - Reprojecting valid tiles layer to match input layer CRS.")
    valid_tiles_sf <- st_transform(valid_tiles_sf, input_crs)
  }
}
target_crs_epsg <- if(!is.na(st_crs(input_crs)$epsg)) st_crs(input_crs)$epsg else "N/A (WKT)"
message("   - CRS checks and transformations complete. Target CRS (EPSG): ", target_crs_epsg)


# --- Preprocessing: Make input geometries valid ---
message("2. Checking and repairing input layer geometries (st_make_valid)...")
rows_before_preproc <- nrow(input_sf)
input_sf <- safe_make_valid(input_sf)
if(nrow(input_sf) < rows_before_preproc) message("   - ", rows_before_preproc - nrow(input_sf), " feature(s) removed due to empty/invalid geometry.")
if (nrow(input_sf) == 0) stop("Input layer is empty or all geometries invalid after repair.")

# --- Step 3: Split, Repair, and Dissolve per label ---
message("3. Splitting and dissolving data per label...")
dissolve_by_label <- function(data, label_value) {
  message("   - Processing label: ", label_value)
  subset_sf <- data %>% filter(Label == label_value)
  if (nrow(subset_sf) == 0) {
    message("     -> No features for this label.")
    return(st_sf(Label = character(), geometry = st_sfc(crs = st_crs(data)))))
  }
  subset_sf <- safe_make_valid(subset_sf)
  if (nrow(subset_sf) == 0) {
    message("     -> No valid features after validation for this label.")
    return(st_sf(Label = character(), geometry = st_sfc(crs = st_crs(data)))))
  }
  dissolved_geom <- tryCatch(st_union(st_geometry(subset_sf)), error = function(e){
    message("     -> Error during st_union for '", label_value, "': ", e$message); st_sfc(crs=st_crs(subset_sf)) })
  if(length(dissolved_geom) == 0 || all(st_is_empty(dissolved_geom))) {
    message("     -> Dissolve resulted in empty geometry for this label.")
    return(st_sf(Label = character(), geometry = st_sfc(crs = st_crs(data)))))
  }
  dissolved_sf <- st_sf(Label = label_value, geometry = dissolved_geom, crs = st_crs(data))
  dissolved_sf <- safe_make_valid(dissolved_sf)
  if (nrow(dissolved_sf) > 0) message("     -> Dissolve completed for '", label_value, "'.")
  else message("     -> Dissolve for '", label_value, "' resulted in empty after final validation.")
  return(dissolved_sf)
}

inundated_dissolved <- dissolve_by_label(input_sf, "inundated")
uncertain_dissolved <- dissolve_by_label(input_sf, "uncertain")
other_dissolved     <- dissolve_by_label(input_sf, "other")

# --- Step 4: Apply Prioritization (Difference) ---
message("4. Applying prioritization (st_difference)... Priority: inundated > other > uncertain")
other_prio <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
uncertain_prio <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)

message("   - Calculating: 'other' minus 'inundated'")
if (nrow(other_dissolved) > 0) {
  if (nrow(inundated_dissolved) > 0 && !all(st_is_empty(inundated_dissolved$geometry))) {
    diff_geom_other <- tryCatch(st_difference(st_geometry(other_dissolved), st_geometry(inundated_dissolved)),
                                error = function(e) { message("     -> Error: ", e$message); st_sfc(crs = input_crs) })
    diff_geom_other <- diff_geom_other[st_geometry_type(diff_geom_other) %in% c("POLYGON", "MULTIPOLYGON")]
    if (length(diff_geom_other) > 0 && !all(st_is_empty(diff_geom_other))) {
      other_prio <- st_sf(geometry = diff_geom_other, crs = input_crs) %>% mutate(Label = "other") %>% safe_make_valid()
    }
  } else { other_prio <- other_dissolved }
}
if(nrow(other_prio) > 0) {
  message("     -> 'other_prio' calculated with ", nrow(other_prio), " features.")
} else {
  message("     -> 'other_prio' is empty.")
}

message("   - Combining: 'inundated' + 'other_prio' for next step")
layers_to_combine_new <- list(inundated_dissolved, other_prio)
valid_geoms_new <- lapply(layers_to_combine_new, function(lyr) {
  if (inherits(lyr, "sf") && nrow(lyr) > 0 && !all(st_is_empty(lyr$geometry))) st_geometry(lyr) else NULL })
valid_geoms_new <- Filter(Negate(is.null), valid_geoms_new)
higher_prio_combined_geom_new <- st_sfc(crs = input_crs)
if (length(valid_geoms_new) > 0) {
  higher_prio_combined_geom_new <- st_union(st_combine(do.call(c, valid_geoms_new))) %>% safe_make_valid()
}
if(!all(st_is_empty(higher_prio_combined_geom_new))) {
  message("     -> 'inundated' + 'other_prio' combined.")
} else {
  message("     -> 'inundated' + 'other_prio' combination is empty.")
}

message("   - Calculating: 'uncertain' minus ('inundated' + 'other_prio')")
if (nrow(uncertain_dissolved) > 0) {
  if (length(higher_prio_combined_geom_new) > 0 && !all(st_is_empty(higher_prio_combined_geom_new))) {
    diff_geom_uncertain <- tryCatch(st_difference(st_geometry(uncertain_dissolved), higher_prio_combined_geom_new),
                                    error = function(e) { message("     -> Error: ", e$message); st_sfc(crs = input_crs) })
    diff_geom_uncertain <- diff_geom_uncertain[st_geometry_type(diff_geom_uncertain) %in% c("POLYGON", "MULTIPOLYGON")]
    if (length(diff_geom_uncertain) > 0 && !all(st_is_empty(diff_geom_uncertain))) {
      uncertain_prio <- st_sf(geometry = diff_geom_uncertain, crs = input_crs) %>% mutate(Label = "uncertain") %>% safe_make_valid()
    }
  } else { uncertain_prio <- uncertain_dissolved }
}
if(nrow(uncertain_prio) > 0) {
  message("     -> 'uncertain_prio' calculated with ", nrow(uncertain_prio), " features.")
} else {
  message("     -> 'uncertain_prio' is empty.")
}

# --- Step 5: Combine final prioritized layers ---
message("5. Combining final prioritized layers (inundated_dissolved, other_prio, uncertain_prio)...")
final_layers_list <- list(inundated_dissolved, other_prio, uncertain_prio)
valid_final_layers <- Filter(function(lyr) inherits(lyr, "sf") && nrow(lyr) > 0 && !all(st_is_empty(lyr$geometry)), final_layers_list)

# --- Main Logic: Process if layers exist, else fill background ---
if (length(valid_final_layers) > 0) {
  message("   - Combining ", length(valid_final_layers), " valid prioritized layer(s) using rbind...")
  final_combined_sf <- do.call(rbind, valid_final_layers) %>%
    select(Label, geometry) %>%
    st_set_crs(input_crs) %>%
    safe_make_valid()
  
  message("6. Converting prioritized layers to singlepart features (st_cast)...")
  final_output_sf <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
  if (nrow(final_combined_sf) > 0) {
    temp_processed_sf <- final_combined_sf
    if (any(st_geometry_type(temp_processed_sf) == "GEOMETRYCOLLECTION")) {
      temp_processed_sf <- st_collection_extract(temp_processed_sf, type = "POLYGON") %>%
        filter(!st_is_empty(geometry), st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
    }
    if (nrow(temp_processed_sf) > 0) {
      final_output_sf <- st_cast(temp_processed_sf, "POLYGON") %>% safe_make_valid()
    }
  }
  if (nrow(final_output_sf) == 0) message("   - Prioritized layers are empty after casting/validation.")
  else message("   - Prioritized layers converted to singlepart: ", nrow(final_output_sf), " features.")
  
  message("6b. Calculating background fill class: '", background_label, "'")
  background_fill_sf <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
  processed_area_geom <- st_sfc(crs = input_crs)
  if(nrow(final_output_sf) > 0) {
    processed_area_geom <- st_union(st_buffer(st_geometry(final_output_sf), 0)) %>% safe_make_valid()
    if(all(st_is_empty(processed_area_geom))) message("    - Union of prioritized areas is empty.")
    else message("    - Union of prioritized areas calculated.")
  } else message("    - No prioritized polygons to calculate processed area from.")
  
  if (use_tile_specific_background) {
    if (!is.null(valid_tiles_sf) && nrow(valid_tiles_sf) > 0) {
      message("   - Calculating background fill for ", nrow(valid_tiles_sf), " specific valid tile(s)...")
      list_of_background_pieces <- lapply(1:nrow(valid_tiles_sf), function(i) {
        current_tile_geom <- st_geometry(valid_tiles_sf[i, ])
        empty_in_tile_geom <- st_sfc(crs = input_crs)
        if (!all(st_is_empty(processed_area_geom))) {
          intersecting_processed <- st_intersection(current_tile_geom, processed_area_geom)
          if (nrow(intersecting_processed) > 0 && !all(st_is_empty(intersecting_processed$geometry))) {
            empty_in_tile_geom <- st_difference(current_tile_geom, st_union(st_geometry(intersecting_processed)))
          } else empty_in_tile_geom <- current_tile_geom
        } else empty_in_tile_geom <- current_tile_geom
        empty_in_tile_geom <- safe_make_valid(empty_in_tile_geom)
        empty_in_tile_geom <- empty_in_tile_geom[st_geometry_type(empty_in_tile_geom) %in% c("POLYGON", "MULTIPOLYGON")]
        if (length(empty_in_tile_geom) > 0 && !all(st_is_empty(empty_in_tile_geom))) {
          return(st_sf(Label = background_label, geometry = empty_in_tile_geom, crs = input_crs))
        } else return(NULL)
      })
      valid_background_pieces <- Filter(Negate(is.null), list_of_background_pieces)
      if (length(valid_background_pieces) > 0) background_fill_sf <- do.call(rbind, valid_background_pieces)
      else message("    - No background areas generated from valid tiles.")
    } else {
      message("   - Tile-specific background is enabled, but NO valid tiles were found. NO background will be generated for prioritized layers.")
    }
  } else { # use_tile_specific_background is FALSE
    message("   - Calculating global background fill (tile-specific disabled) using global dissolved grid.")
    current_boundary_for_bg <- st_geometry(global_dissolved_grid_sf)
    empty_space_geom_global <- st_sfc(crs = input_crs)
    if (!all(st_is_empty(processed_area_geom))) {
      empty_space_geom_global <- st_difference(current_boundary_for_bg, processed_area_geom)
    } else empty_space_geom_global <- current_boundary_for_bg
    empty_space_geom_global <- safe_make_valid(empty_space_geom_global)
    empty_space_geom_global <- empty_space_geom_global[st_geometry_type(empty_space_geom_global) %in% c("POLYGON", "MULTIPOLYGON")]
    if (length(empty_space_geom_global) > 0 && !all(st_is_empty(empty_space_geom_global))) {
      background_fill_sf <- st_sf(Label = background_label, geometry = empty_space_geom_global, crs = input_crs)
    } else message("    - Global background calculation resulted in empty geometry.")
  }
  
  if (nrow(background_fill_sf) > 0) {
    background_fill_sf <- safe_make_valid(background_fill_sf)
    if (nrow(background_fill_sf) > 0) {
      message("    - Casting background polygons to singlepart...")
      background_fill_sf <- st_cast(background_fill_sf, "POLYGON") %>% safe_make_valid()
      message("    - Background layer has ", nrow(background_fill_sf), " polygon(s) before sliver removal.")
      if(remove_slivers && nrow(background_fill_sf) > 0) {
        message("    - Removing '", background_label, "' slivers smaller than ", area_threshold, " sq. units...")
        rows_before <- nrow(background_fill_sf)
        background_fill_sf <- background_fill_sf %>%
          mutate(area_calc = st_area(.), area_sq_units_num = as.numeric(area_calc)) %>%
          filter(area_sq_units_num >= area_threshold) %>%
          select(-area_calc, -area_sq_units_num)
        if(nrow(background_fill_sf) < rows_before) message("      -> Removed ", rows_before - nrow(background_fill_sf), " sliver(s).")
        else message("      -> No slivers found below threshold.")
      }
    } else message("    - Background fill layer became empty after initial validation (before cast/sliver).")
  } else message("    - Background fill layer is empty or was not generated (before cast/sliver).")
  
  if (nrow(background_fill_sf) > 0) {
    final_object_to_write <- rbind(final_output_sf, background_fill_sf)
    message("   - Combined prioritized layers with '", background_label, "' background.")
  } else {
    final_object_to_write <- final_output_sf
    message("   - No background generated or it was empty; using only prioritized layers.")
  }
  final_object_to_write <- final_object_to_write %>% filter(nrow(.) > 0)
  message("Script completed processing prioritized layers path.")
} else { # This 'else' is for: if (length(valid_final_layers) == 0)
  message("No valid prioritized layers found after Step 5. Processing background-only.")
  final_object_to_write <- st_sf(Label = character(), geometry = st_sfc(), crs = input_crs)
  
  if (use_tile_specific_background) {
    if (!is.null(valid_tiles_sf) && nrow(valid_tiles_sf) > 0) {
      message("   - Filling ", nrow(valid_tiles_sf), " valid tile(s) as '", background_label, "'...")
      final_object_to_write <- valid_tiles_sf %>%
        mutate(Label = background_label) %>%
        select(Label, geometry) %>%
        st_set_crs(input_crs) %>%
        safe_make_valid()
    } else {
      message("   - Tile-specific background-only fill is enabled, but NO valid tiles were found. Output will be empty.")
    }
  } else { # use_tile_specific_background is FALSE
    message("   - Filling global dissolved grid boundary as '", background_label, "' (tile-specific disabled).")
    final_object_to_write <- global_dissolved_grid_sf %>%
      mutate(Label = background_label) %>%
      select(Label, geometry) %>%
      st_set_crs(input_crs) %>%
      safe_make_valid()
  }
  
  if (nrow(final_object_to_write) > 0) {
    message("    - Casting background-only polygons to singlepart...")
    final_object_to_write <- st_cast(final_object_to_write, "POLYGON") %>% safe_make_valid()
    message("    - Background-only layer has ", nrow(final_object_to_write), " polygon(s) before sliver removal.")
    if(remove_slivers && nrow(final_object_to_write) > 0) {
      message("    - Removing '", background_label, "' slivers smaller than ", area_threshold, " sq. units...")
      rows_before <- nrow(final_object_to_write)
      final_object_to_write <- final_object_to_write %>%
        mutate(area_calc = st_area(.), area_sq_units_num = as.numeric(area_calc)) %>%
        filter(area_sq_units_num >= area_threshold) %>%
        select(-area_calc, -area_sq_units_num)
      if(nrow(final_object_to_write) < rows_before) message("      -> Removed ", rows_before - nrow(final_object_to_write), " sliver(s).")
      else message("      -> No slivers found below threshold.")
    }
  } else message("    - Background-only layer is empty or was not generated after initial processing.")
  message("Script completed background-only path.")
}

# --- Step 7: Write Output ---
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
  message("Attempting to create output directory: ", output_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
}

if (exists("final_object_to_write") && inherits(final_object_to_write, "sf") && nrow(final_object_to_write) > 0 && dir.exists(output_dir)) {
  message("7. Writing final result (", nrow(final_object_to_write), " features) to: ", output_file_path)
  tryCatch({
    st_write(final_object_to_write, output_file_path, delete_layer = TRUE)
    message("   - Write completed successfully.")
  }, error = function(e) warning("Error writing final output file: ", e$message))
} else if (!dir.exists(output_dir)){
  warning("Output directory could not be found or created: ", output_dir, ". Result NOT written.")
} else {
  message("7. Final result is empty or invalid, no output file written.")
}

message("--- Script Finished ---")
