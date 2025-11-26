# --- Raster Processing Utility Functions ---

#' Create a Multi-Layer Fractional Cover Raster
#'
#' Converts a vector polygon file into a multi-layer raster where each layer
#' represents the fractional cover of a specific label. It includes a final
#' step to mask out pixels that are not 100% covered by the input polygons.
#'
#' @param polygons_sf An `sf` object containing polygons with a 'Label' column.
#' @param template_raster A `SpatRaster` object from the 'terra' package to use as the grid template.
#' @return A multi-layer `SpatRaster` object. Pixels not fully covered by labels will have NA values.
create_fraction_raster <- function(polygons_sf, template_raster) {
  # Ensure required packages are available
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  
  labels <- unique(polygons_sf$Label)
  message(paste("Found", length(labels), "unique labels to rasterize:", paste(labels, collapse = ", ")))
  
  fraction_layers <- list()
  for (label in labels) {
    message(paste(" -> Rasterizing label:", label))
    label_polygons <- polygons_sf[polygons_sf$Label == label, ]
    # Rasterize, calculating the fractional coverage of each pixel
    fraction_layer <- terra::rasterize(label_polygons, template_raster, cover = TRUE, background = 0)
    fraction_layers[[label]] <- fraction_layer
  }
  
  message("Stacking all fraction layers into a single SpatRaster...")
  combined_raster <- terra::rast(fraction_layers)
  
  # --- NEW: MASK PIXELS THAT ARE NOT FULLY COVERED ---
  message("Creating a mask for pixels not fully covered by labels...")
  
  # 1. Calculate the sum of all fraction layers for each pixel.
  pixel_sums <- sum(combined_raster)
  
  # 2. Create a mask. Pixels are kept (value = 1) only if their sum is
  #    approximately 1. We use a small tolerance for floating-point precision.
  #    All other pixels (not fully covered) will become NA.
  mask_layer <- terra::app(pixel_sums, fun = function(x) {
    ifelse(abs(x - 1) < 1e-6, 1, NA)
  })
  
  # 3. Apply this mask to the original multi-layer raster.
  #    This sets all bands to NA for partially covered pixels.
  final_masked_raster <- terra::mask(combined_raster, mask_layer)
  
  message("Masking complete. Partially covered pixels have been set to NA.")
  return(final_masked_raster)
}

#' Verify geometric compatibility between two rasters
#'
#' Checks if two rasters share the same CRS, resolution, and origin, which is
#' essential for reliable cell-based data extraction.
#'
#' @param raster1 A SpatRaster object from the 'terra' package.
#' @param raster2 A second SpatRaster object.
#' @return Returns TRUE if compatible, otherwise stops with an error.
verify_raster_compatibility <- function(raster1, raster2) {
  # Ensure the terra package is available
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  
  message("Verifying geometric compatibility (CRS, Resolution, Origin)...")
  compatible_crs <- terra::compareGeom(raster1, raster2, crs = TRUE, res = FALSE, stopOnError = FALSE)
  compatible_res <- all(terra::res(raster1) == terra::res(raster2))
  origin_diff <- abs(terra::origin(raster1) - terra::origin(raster2))
  compatible_origin <- all(origin_diff < (min(terra::res(raster1)) * 1e-3)) # Tolerance
  
  if (compatible_crs && compatible_res && compatible_origin) {
    message("SUCCESS: Rasters are geometrically compatible.")
    return(TRUE)
  } else {
    stop(paste(
      "FATAL: Rasters are not compatible.",
      "\n  CRS Match:", compatible_crs,
      "\n  Resolution Match:", compatible_res,
      "\n  Origin Match:", compatible_origin
    ))
    return(FALSE)
  }
}


#' Extract pixel data from two compatible rasters
#'
#' Extracts co-located pixel values from two rasters based on the valid
#' (non-NA) cells in the first raster.
#'
#' @param fractions_raster The primary SpatRaster (e.g., fractional cover).
#' @param s2_raster The secondary SpatRaster (e.g., Sentinel-2 bands).
#' @return A data frame combining coordinates, s2 values, and fraction values.
extract_pixel_data <- function(fractions_raster, s2_raster) {
  # Ensure required packages are available
  if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required.")
  
  # 1. Identify valid cells from the fractions raster
  message("Identifying valid (non-NA) pixels...")
  valid_cells <- which(!is.na(terra::values(fractions_raster[[1]], mat = FALSE)))
  if (length(valid_cells) == 0) stop("No valid pixels found in fractions raster.")
  message(paste("Found", length(valid_cells), "valid pixels."))
  
  # 2. Extract values and coordinates
  message("Extracting values from both rasters...")
  fraction_values <- terra::extract(fractions_raster, valid_cells)
  s2_values <- terra::extract(s2_raster, valid_cells)
  coords <- terra::xyFromCell(fractions_raster, valid_cells)
  
  # 3. Combine into a single data frame
  message("Combining all extracted data...")
  colnames(s2_values) <- paste0("S2_", names(s2_raster))
  combined_df <- cbind(as.data.frame(coords), s2_values, fraction_values)
  
  message("Data extraction and combination complete.")
  return(combined_df)
}

#' Create an Ordered Bar Plot of Area
#'
#' Generates a ggplot bar chart from a summary data frame, ordered by area,
#' suitable for visualizing habitat distribution.
#'
#' @param data A data frame with columns `HAB1` and `Total_Area_ha`.
#' @param category_name A string for the plot title (e.g., "Grassland").
#' @param study_site A string with the name of the study site for the title.
#' @return A `ggplot` object.
plot_bar_chart <- function(data, category_name, study_site) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  
  ggplot2::ggplot(data, ggplot2::aes(x = reorder(HAB1, Total_Area_ha), y = Total_Area_ha, fill = Total_Area_ha)) +
    ggplot2::geom_bar(stat = "identity", show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(Total_Area_ha, 1), " ha")), hjust = -0.1, size = 3.5) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Total Area per HAB1 (", category_name, ") in", study_site),
      x = "HAB1 Class",
      y = "Total Area (ha)"
    )
}


#' Save and Upload a Filtered GeoPackage
#'
#' Filters an `sf` object based on the first letter of the 'EENH1' column,
#' saves the result as a GeoPackage (.gpkg), and uploads it to Google Drive.
#'
#' @param data An `sf` object with an 'EENH1' column.
#' @param letter A character (e.g., "h" or "m") to filter by.
#' @param site_name The name of the study site for the output filename.
#' @param local_dir The local directory to save the .gpkg file.
#' @param gdrive_folder_id The Google Drive folder ID to upload the file to.
#' @return Invisibly returns TRUE on success.
save_filtered_geopackage <- function(data, letter, site_name, local_dir, gdrive_folder_id) {
  # Ensure required packages are available
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  
  data <- dplyr::mutate(data, EENH1_Group = stringr::str_to_lower(stringr::str_sub(EENH1, 1, 1)))
  filtered_data <- dplyr::filter(data, EENH1_Group == letter)
  
  if (nrow(filtered_data) > 0) {
    gpkg_filename <- paste0(site_name, "_BWK2023_", letter, ".gpkg")
    gpkg_path <- file.path(local_dir, gpkg_filename)
    sf::st_write(filtered_data, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
    message("Uploading GeoPackage: ", gpkg_filename)
    googledrive::drive_upload(media = gpkg_path, path = googledrive::as_id(gdrive_folder_id), overwrite = TRUE)
  } else {
    message("No features found for EENH1 group '", letter, "' to save.")
  }
  invisible(TRUE)
}


#' Calculate common spectral indices from a data frame
#'
#' @param df A data frame containing scaled Sentinel-2 bands (e.g., 'B03_scaled').
#' @return The data frame with added columns for each spectral index.
calculate_spectral_indices <- function(df) {
  # Helper function for STR, expects scaled values
  swir_to_str <- function(swir) {
    ifelse(is.na(swir) | swir <= 0, NA_real_, ((1 - swir)^2) / (2 * swir))
  }
  
  df %>%
    mutate(
      ndvi = (.data[["B8A_scaled"]] - .data[["B04_scaled"]]) / (.data[["B8A_scaled"]] + .data[["B04_scaled"]]),
      ndwi_mf = (.data[["B03_scaled"]] - .data[["B8A_scaled"]]) / (.data[["B03_scaled"]] + .data[["B8A_scaled"]]),
      mndwi11 = (.data[["B03_scaled"]] - .data[["B11_scaled"]]) / (.data[["B03_scaled"]] + .data[["B11_scaled"]]),
      mndwi12 = (.data[["B03_scaled"]] - .data[["B12_scaled"]]) / (.data[["B03_scaled"]] + .data[["B12_scaled"]]),
      ndmi_gao11 = (.data[["B8A_scaled"]] - .data[["B11_scaled"]]) / (.data[["B8A_scaled"]] + .data[["B11_scaled"]]),
      str1 = swir_to_str(.data[["B11_scaled"]]),
      str2 = swir_to_str(.data[["B12_scaled"]])
    )
}

#' Classify pixels based on land cover fraction purity
#'
#' @param df A data frame containing fractional cover columns.
#' @param fraction_colnames A character vector of the column names representing fractions.
#' @return The data frame with two new, correctly ordered factor columns: 
#'   'dominant_label' and 'mixture_category'.
classify_pixel_mixture <- function(df, fraction_colnames) {
  pixel_fractions <- df[, fraction_colnames, drop = FALSE]
  
  # Get the name of the dominant fraction for each pixel (row)
  dominant_label_character <- fraction_colnames[max.col(pixel_fractions, ties.method = "first")]
  
  # Convert to a factor, using the EXACT order from the fraction raster layers.
  # This prevents alphabetical re-sorting and fixes the label shift.
  df$dominant_label <- factor(dominant_label_character, levels = fraction_colnames)
  
  # Get the value of the dominant fraction
  max_fraction <- do.call(pmax, c(pixel_fractions, na.rm = TRUE))
  
  # Classify the pixel based on purity
  mixture_category_character <- case_when(
    max_fraction >= 0.9 ~ "pure",
    max_fraction > 0.6  ~ "mixed",
    TRUE                ~ "very_mixed"
  )
  
  # **FIX 2**: Convert mixture category to a factor with a defined order.
  # This ensures plots and tables are always ordered logically.
  df$mixture_category <- factor(mixture_category_character, levels = c("pure", "mixed", "very_mixed"))
  
  return(df)
}

#' Create Raster Layers from Data Frame Columns
#'
#' Converts specified columns of a data frame back into raster layers using a
#' template raster for the correct geometry. Handles both numeric and categorical data.
#'
#' @param df The data frame containing pixel data and coordinates (x_coord, y_coord).
#' @param columns_to_rasterize A character vector of column names to convert to raster layers.
#' @param template_raster A `SpatRaster` to provide the grid geometry (CRS, extent, resolution).
#' @return A multi-layer `SpatRaster` object containing the new layers.
rasterize_attributes <- function(df, columns_to_rasterize, template_raster) {
  # Get the cell indices from the coordinates once, as it's the same for all layers.
  cells <- terra::cellFromXY(template_raster, df[, c("x_coord", "y_coord")])
  
  new_layers <- list()
  
  for (col_name in columns_to_rasterize) {
    message(paste(" -> Rasterizing attribute:", col_name))
    
    # Create an empty raster layer with the correct geometry.
    new_layer <- terra::rast(template_raster, nlyr = 1)
    names(new_layer) <- col_name
    
    # Get the values for the current column.
    values_to_rasterize <- df[[col_name]]
    
    # Check if the column is categorical (character or factor).
    if (is.character(values_to_rasterize) || is.factor(values_to_rasterize)) {
      # For categorical data, convert to a factor to get numeric levels.
      factor_values <- as.factor(values_to_rasterize)
      
      # Assign the numeric factor levels to the raster cells.
      new_layer[cells] <- as.numeric(factor_values)
      
      # Attach the text labels to the raster layer for GIS software.
      levels(new_layer) <- levels(factor_values)
      
    } else {
      # For numeric data (like NDVI), assign the values directly.
      new_layer[cells] <- values_to_rasterize
    }
    
    new_layers[[col_name]] <- new_layer
  }
  
  return(terra::rast(new_layers))
}