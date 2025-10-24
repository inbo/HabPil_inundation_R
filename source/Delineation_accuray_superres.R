# ==============================================================================
# Quantitative Comparison of Delineation Accuracy (IoU & Dice)
# ------------------------------------------------------------------------------
# Purpose:
# This script compares the delineation accuracy of raster classification maps
# (original 10m and superres 1m) against reference polygon labels for
# 'Inundated' areas using Intersection over Union (IoU) and Dice Coefficient.
# It masks the analysis to only areas labeled as 'Inundated' or 'Not inundated'
# in the reference data.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(units)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
# study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
 study_site_name <- "Webbekomsbroek2"

# target_year <- 2020
# target_year <- 2021
# target_year <- 2023
 target_year <- 2024
 
 # --- Root Directories ---
 data_root_dir <- "data"
 output_root_dir <- "output"
 
 # --- Input File Paths ---
 ref_polygon_path <- file.path(
   data_root_dir, "final_labels",
   if (study_site_name == "Webbekomsbroek2") {
     paste0("Labels_WB_", target_year, "_2.shp")
   } else {
     site_abbr <- case_when(
       study_site_name == "Webbekomsbroek" ~ "WB",
       study_site_name == "Schulensmeer" ~ "SM",
       study_site_name == "Kloosterbeemden" ~ "KB",
       TRUE ~ NA_character_
     )
     paste0("Labels_", site_abbr, "_", target_year, ".shp")
   }
 )
 # Choose Jussila or WiW model outputs
 class_map_orig_path <- file.path(
   output_root_dir, "classified_maps", study_site_name,
   paste0(study_site_name, "_", target_year, "_jussila_classified_map.tif")
 )
 class_map_superres_path <- file.path(
   output_root_dir, "classified_maps", study_site_name,
   paste0(study_site_name, "_", target_year, "_jussila_classified_map_superres.tif")
 )
 
 # --- Output Directories ---
 masked_output_dir <- file.path(output_root_dir, "masked_rasters", study_site_name, target_year)
 dir.create(masked_output_dir, recursive = TRUE, showWarnings = FALSE)
 summary_output_dir <- file.path(output_root_dir, "summary_plots") # Defined even if plotting removed
 dir.create(summary_output_dir, recursive = TRUE, showWarnings = FALSE)
 metrics_output_dir <- file.path(output_root_dir, "pixel_data_tables", study_site_name, target_year)
 dir.create(metrics_output_dir, recursive = TRUE, showWarnings = FALSE)
 
 
 # ==============================================================================
 # 1️⃣ Load Data & Harmonize CRS
 # ==============================================================================
 message("\n--- Loading Data and Harmonizing CRS ---")
 
 # --- Load Data ---
 if (!file.exists(ref_polygon_path)) stop("Ref shapefile not found: ", ref_polygon_path)
 ref_polygons_all <- st_read(ref_polygon_path, quiet = TRUE)
 if (!file.exists(class_map_orig_path)) stop("Orig map not found: ", class_map_orig_path)
 class_map_orig <- rast(class_map_orig_path)
 if (!file.exists(class_map_superres_path)) stop("Superres map not found: ", class_map_superres_path)
 class_map_superres <- rast(class_map_superres_path)
 message("Data loaded.")
 
 # --- Determine & Harmonize Target CRS ---
 target_crs_terra <- if (terra::same.crs(crs(class_map_orig), crs(class_map_superres)) && !terra::is.lonlat(crs(class_map_orig))) {
   crs(class_map_orig)
 } else if (!st_is_longlat(ref_polygons_all)) {
   crs(st_crs(ref_polygons_all)$wkt) # Convert sf CRS to terra CRS
 } else {
   warning("Defaulting to EPSG:32631. Adjust if needed.")
   crs("EPSG:32631")
 }
 target_crs_sf <- st_crs(target_crs_terra) # Ensure sf CRS matches
 message(paste("Target CRS set to:", terra::crs(target_crs_terra, proj=TRUE)))
 if (st_crs(ref_polygons_all) != target_crs_sf) {
   message("Reprojecting reference polygons..."); ref_polygons_all <- st_transform(ref_polygons_all, target_crs_sf) }
 if (!terra::same.crs(crs(class_map_orig), target_crs_terra)) {
   message("Reprojecting original map..."); class_map_orig <- project(class_map_orig, target_crs_terra, method="near") }
 if (!terra::same.crs(crs(class_map_superres), target_crs_terra)) {
   message("Reprojecting superres map..."); class_map_superres <- project(class_map_superres, target_crs_terra, method="near") }
 message("CRS Harmonized.")
 
 # --- Filter Reference Polygons, Clean, Extract Polygons, and Buffer ---
 ref_polygons_filtered <- ref_polygons_all %>%
   filter(Label %in% c("Inundated", "Not inundated")) %>%
   mutate(ref_value = if_else(Label == "Inundated", 2L, 1L)) %>%
   st_make_valid() %>%
   st_collection_extract("POLYGON") %>%
   st_buffer(dist = 0) # Added zero buffer
 
 if (!inherits(ref_polygons_filtered, "sf") || nrow(ref_polygons_filtered) == 0) {
   stop("No relevant reference polygons found after filtering/cleaning/buffering.")
 }
 message("Reference polygons filtered, cleaned, extracted, and buffered.")
 
 # --- Create Mask and Isolate Inundated Polygons (Cleaned + Buffered) ---
 reference_mask <- st_union(ref_polygons_filtered) %>% st_make_valid() %>% st_collection_extract("POLYGON") %>% st_buffer(dist = 0) %>% st_sf()
 if (nrow(reference_mask) == 0 || st_is_empty(reference_mask)) stop("Reference mask became empty after cleaning/buffering.")
 
 ref_inundated_polygons <- ref_polygons_filtered %>%
   filter(Label == "Inundated") %>% st_union() %>% st_make_valid() %>% st_collection_extract("POLYGON") %>% st_buffer(dist = 0) %>% st_sf()
 if (nrow(ref_inundated_polygons) == 0 || st_is_empty(ref_inundated_polygons)) warning("No valid 'Inundated' reference polygons remain after cleaning/buffering.")
 
 message("Reference mask and inundated polygons prepared.")
 
 
 # ==============================================================================
 # 2️⃣ Generate and Save Masked Rasters (No Plotting)
 # ==============================================================================
 message("\n--- Generating and Saving Masked Rasters ---")
 
 # --- Process Original Resolution (10m) ---
 message("Processing Original Resolution (10m)...")
 # Rasterize reference (Input is now clean polygons)
 ref_raster_10m <- tryCatch(
   rasterize(vect(ref_polygons_filtered), class_map_orig, field = "ref_value", background = NA, touches = TRUE), # Direct vect()
   error = function(e){ warning("Failed rasterize reference (10m): ", e$message); NULL }
 )
 if(!is.null(ref_raster_10m)) names(ref_raster_10m) <- "Reference_10m"
 
 # Rasterize mask (Input is now clean polygons)
 raster_mask_10m <- tryCatch(
   rasterize(vect(reference_mask), class_map_orig, background=NA), # Direct vect()
   error=function(e){warning("Failed rasterize mask (10m): ", e$message); NULL}
 )
 # Mask classification
 class_map_masked_10m <- if (!is.null(raster_mask_10m) && !all(is.na(minmax(raster_mask_10m)))) mask(class_map_orig, raster_mask_10m) else rast(class_map_orig)*NA
 names(class_map_masked_10m) <- "Classification_10m"
 message("Masked 10m rasters created.")
 
 # --- Process Super Resolution (1m) ---
 message("\nProcessing Super Resolution (1m)...")
 # Rasterize mask (Input is now clean polygons)
 raster_mask_1m <- tryCatch(
   rasterize(vect(reference_mask), class_map_superres, background=NA), # Direct vect()
   error=function(e){warning("Failed rasterize mask (1m): ", e$message); NULL}
 )
 # Mask classification
 class_map_masked_1m <- if (!is.null(raster_mask_1m) && !all(is.na(minmax(raster_mask_1m)))) mask(class_map_superres, raster_mask_1m) else rast(class_map_superres)*NA
 names(class_map_masked_1m) <- "Classification_1m"
 message("Masked 1m raster created.")
 
 # --- Save Individual Masked Rasters ---
 message("\nSaving individual masked rasters...")
 if (!is.null(ref_raster_10m) && !all(is.na(minmax(ref_raster_10m)))) {
   writeRaster(ref_raster_10m, file.path(masked_output_dir, paste0(study_site_name, "_", target_year, "_reference_masked_10m.tif")), overwrite = TRUE, wopt=list(gdal=c("COMPRESS=LZW"), datatype="INT1U"))
 } else { message("Skipping save for empty/failed reference_masked_10m.")}
 
 if (!all(is.na(minmax(class_map_masked_10m)))) {
   writeRaster(class_map_masked_10m, file.path(masked_output_dir, paste0(study_site_name, "_", target_year, "_classification_masked_10m.tif")), overwrite = TRUE, wopt=list(gdal=c("COMPRESS=LZW"), datatype="INT1U"))
 } else { message("Skipping save for empty/failed classification_masked_10m.")}
 
 if (!all(is.na(minmax(class_map_masked_1m)))) {
   writeRaster(class_map_masked_1m, file.path(masked_output_dir, paste0(study_site_name, "_", target_year, "_classification_masked_1m.tif")), overwrite = TRUE, wopt=list(gdal=c("COMPRESS=LZW"), datatype="INT1U"))
 } else { message("Skipping save for empty/failed classification_masked_1m.")}
 
 message("Individual masked rasters saved (if they contained data) to: ", masked_output_dir)
 
 
 # ==============================================================================
 # 3️⃣ Define Helper Functions for Metrics
 # ==============================================================================
 
 #' Process Classification Raster: Polygonize 'Water' from MASKED raster
 process_masked_classification_poly <- function(class_map_masked, crs_sf) {
   # Ensure crs_sf is a valid sf crs object
   if (!inherits(crs_sf, "crs")) { stop("crs_sf argument must be an sf CRS object.") }
   # Check if input raster is valid
   if (!inherits(class_map_masked, "SpatRaster") || ncell(class_map_masked) == 0 || all(is.na(minmax(class_map_masked)))) { message("Input masked raster is invalid or empty."); return(st_sf(geometry = st_sfc(), crs = crs_sf)) }
   message("\n--- Polygonizing masked raster: ", names(class_map_masked), " ---")
   message("Reclassifying to isolate 'water' class...")
   rcl_matrix <- matrix(c(1, NA, 2, 1), ncol = 2, byrow = TRUE)
   water_raster <- tryCatch( classify(class_map_masked, rcl = rcl_matrix), error = function(e){ warning("Error during classify: ", e$message); return(NULL) })
   if(is.null(water_raster)) return(st_sf(geometry = st_sfc(), crs = crs_sf))
   message("Converting 'water' pixels to polygons...")
   minmax_water <- minmax(water_raster)
   if (is.na(minmax_water[2,1]) || minmax_water[2,1] == 0) { message("No 'water' pixels found."); return(st_sf(geometry = st_sfc(), crs = crs_sf)) }
   water_polygons_sv <- tryCatch(as.polygons(water_raster), error = function(e){ warning("Error during as.polygons: ", e$message); return(NULL) })
   if(is.null(water_polygons_sv)) return(st_sf(geometry = st_sfc(), crs = crs_sf))
   water_polygons_sf <- st_as_sf(water_polygons_sv) %>% rename_with(~"layer", 1) %>% filter(!is.na(layer)) %>% st_union() %>% st_make_valid() %>% st_sf() # Ensure sf object
   message("Generated 'water' polygons.")
   return(water_polygons_sf)
 }
 
 #' Calculate IoU and Dice Coefficient
 calculate_overlap_metrics <- function(poly1, poly2) {
   # (Using the most robust version)
   message("Calculating overlap metrics...")
   if (!inherits(poly1, "sf") || !inherits(poly2, "sf")) { warning("Input is not an sf object."); return(list(IoU = NA_real_, Dice = NA_real_)) }
   poly1 <- st_make_valid(poly1); poly2 <- st_make_valid(poly2)
   safe_is_empty <- function(p) { if (nrow(p) == 0) return(TRUE); status <- tryCatch(st_is_empty(p), error = function(e) {warning("st_is_empty error: ", e$message); return(NA)}); return(any(is.na(status) | status)) }
   poly1_is_effectively_empty <- safe_is_empty(poly1); poly2_is_effectively_empty <- safe_is_empty(poly2)
   if (poly1_is_effectively_empty || poly2_is_effectively_empty) { warning("One or both polygon sets are effectively empty."); return(list(IoU = NA_real_, Dice = NA_real_)) }
   intersection_poly <- tryCatch( st_intersection(poly1, poly2), error = function(e){ warning("st_intersection error: ", e$message); return(st_sf(geometry = st_sfc(), crs = st_crs(poly1))) })
   intersection_is_empty <- safe_is_empty(intersection_poly)
   intersection_area <- if (!intersection_is_empty) st_area(intersection_poly) else units::set_units(0, "m^2")
   poly1_area <- st_area(poly1); poly2_area <- st_area(poly2)
   if (is.na(poly1_area) || is.na(poly2_area) || is.na(intersection_area)){ warning("Area calculation resulted in NA."); return(list(IoU = NA_real_, Dice = NA_real_)) }
   union_area <- poly1_area + poly2_area - intersection_area
   iou <- if (union_area > units::set_units(0,"m^2")) as.numeric(intersection_area / union_area) else 0
   dice <- if ((poly1_area + poly2_area) > units::set_units(0,"m^2")) as.numeric((2 * intersection_area) / (poly1_area + poly2_area)) else 0
   iou <- round(iou, 4); dice <- round(dice, 4)
   message(paste("  IoU:", iou)); message(paste("  Dice:", dice))
   return(list(IoU = iou, Dice = dice))
 }
 
 # ==============================================================================
 # 4️⃣ Process Masked Classifications and Calculate Overlap Metrics
 # ==============================================================================
 message("\n--- Processing Masked Classifications and Calculating Metrics ---")
 
 # --- Process Original Resolution ---
 water_polygons_orig <- process_masked_classification_poly(
   class_map_masked = class_map_masked_10m,
   crs_sf = target_crs_sf
 )
 metrics_orig <- calculate_overlap_metrics(water_polygons_orig, ref_inundated_polygons)
 
 # --- Process Super Resolution ---
 water_polygons_superres <- process_masked_classification_poly(
   class_map_masked = class_map_masked_1m,
   crs_sf = target_crs_sf
 )
 metrics_superres <- calculate_overlap_metrics(water_polygons_superres, ref_inundated_polygons)
 
 # ==============================================================================
 # 5️⃣ Calculate Relative Area Error
 # ==============================================================================
 message("\n--- Calculating Relative Error in Total Water Area ---")
 total_mask_area <- st_area(reference_mask)
 ref_water_area <- if (!st_is_empty(ref_inundated_polygons)) st_area(ref_inundated_polygons) else units::set_units(0, "m^2")
 pred_water_area_orig <- if (!st_is_empty(water_polygons_orig)) st_area(water_polygons_orig) else units::set_units(0, "m^2")
 pred_water_area_superres <- if (!st_is_empty(water_polygons_superres)) st_area(water_polygons_superres) else units::set_units(0, "m^2")
 calc_perc <- function(area, total) if (total > units::set_units(0,"m^2")) as.numeric(area/total) else 0
 ref_water_percent <- calc_perc(ref_water_area, total_mask_area)
 pred_water_percent_orig <- calc_perc(pred_water_area_orig, total_mask_area)
 pred_water_percent_superres <- calc_perc(pred_water_area_superres, total_mask_area)
 relative_error_orig <- if (ref_water_percent > 1e-9) (pred_water_percent_orig - ref_water_percent) / ref_water_percent else NA_real_
 relative_error_superres <- if (ref_water_percent > 1e-9) (pred_water_percent_superres - ref_water_percent) / ref_water_percent else NA_real_
 message(paste("  Ref Water Area (%):", round(ref_water_percent * 100, 2), "%"))
 message(paste("  Pred Water Orig (%):", round(pred_water_percent_orig * 100, 2), "%"))
 message(paste("  Pred Water SuperRes (%):", round(pred_water_percent_superres * 100, 2), "%"))
 message(paste("  Rel Error Orig:", round(relative_error_orig * 100, 2), "%"))
 message(paste("  Rel Error SuperRes:", round(relative_error_superres * 100, 2), "%"))
 
 # ==============================================================================
 # 6️⃣ Display Summary and Save Metrics
 # ==============================================================================
 message("\n--- Delineation Accuracy and Area Error Summary ---")
 summary_df <- tibble(
   study_site = study_site_name, year = target_year,
   Resolution = c("Original_10m", "SuperRes_1m"),
   IoU = c(metrics_orig$IoU, metrics_superres$IoU),
   Dice_Coefficient = c(metrics_orig$Dice, metrics_superres$Dice),
   Relative_Area_Error_Percent = c(relative_error_orig * 100, relative_error_superres * 100)
 )
 print(summary_df)
 output_filename <- file.path( metrics_output_dir, paste0(study_site_name, "_", target_year, "_delineation_and_area_metrics.csv"))
 write.csv(summary_df, output_filename, row.names = FALSE)
 message(paste("\nSUCCESS: Combined metrics saved to:\n", output_filename))
 area_percentages_df <- tibble(
   study_site = study_site_name, year = target_year,
   Reference_Water_Percent = ref_water_percent * 100,
   Predicted_Orig_Water_Percent = pred_water_percent_orig * 100,
   Predicted_SuperRes_Water_Percent = pred_water_percent_superres * 100
 )
 area_perc_filename <- file.path( metrics_output_dir, paste0(study_site_name, "_", target_year, "_water_area_percentages.csv"))
 write.csv(area_percentages_df, area_perc_filename, row.names = FALSE)
 message(paste("SUCCESS: Area percentages saved to:\n", area_perc_filename))
 
 