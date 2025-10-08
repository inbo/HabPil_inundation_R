# ==============================================================================
# Download Sentinel-2 Imagery via OpenEO
# ------------------------------------------------------------------------------
# Purpose:
# This script connects to the Copernicus Data Space Ecosystem OpenEO backend
# to download a Sentinel-2 L2A image for a specific date and a user-provided
# area of interest (AOI) defined by a shapefile.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(openeo)
library(sf)
library(terra)
library(lubridate) # For easily extracting the year from the date

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- USER-DEFINED PARAMETERS ---

# 1. Define the full name of your study site (used for output filenames)
study_site_name <- "Webbekomsbroek"

# 2. Provide the exact date for the Sentinel-2 image
target_date <- "2025-09-28"

# 3. Provide the full path to your local boundary shapefile
boundary_shp_file_path <- "C:/Users/stien_heremans/Downloads/refieldvisitdemoworkshophabitatpilot/Webbekom_testsite.shp" 

# 4. Define the local directory where the final Sentinel-2 image will be saved
output_dir <- "data/Sen2_workshop"


# --- OPENEO PARAMETERS ---

# OpenEO backend URL
openeo_backend_url <- "https://openeo.dataspace.copernicus.eu"

# Output format for the downloaded raster
openeo_output_format <- "GTiff"


# ==============================================================================
# 1. Load and Prepare Area of Interest (AOI)
# ==============================================================================
message("\n--- Loading and preparing the Area of Interest ---")

# Check if the shapefile exists
if (!file.exists(boundary_shp_file_path)) {
  stop("FATAL: Boundary shapefile not found at: \n", boundary_shp_file_path)
}

# Load the boundary shapefile
aoi_polygon <- st_read(boundary_shp_file_path, quiet = TRUE)
message("Boundary shapefile loaded successfully.")

# Transform the polygon's coordinate reference system (CRS) to WGS84 (EPSG:4326) for OpenEO
aoi_polygon_wgs84 <- st_transform(aoi_polygon, crs = 4326)
message("Transformed AOI to WGS84 for OpenEO compatibility.")

# Extract the bounding box from the transformed polygon
aoi_bbox_wgs84 <- st_bbox(aoi_polygon_wgs84)

# Format the bounding box for the OpenEO `load_collection` process
aoi_openeo <- list(
  west = as.numeric(aoi_bbox_wgs84["xmin"]),
  south = as.numeric(aoi_bbox_wgs84["ymin"]),
  east = as.numeric(aoi_bbox_wgs84["xmax"]),
  north = as.numeric(aoi_bbox_wgs84["ymax"])
)
message("Defined OpenEO bounding box from shapefile.")

# ==============================================================================
# 2. Connect to OpenEO and Define Job
# ==============================================================================
message("\n--- Connecting to OpenEO and defining the job ---")

# Connect to the OpenEO backend
con <- connect(openeo_backend_url)
login()
message("OpenEO login successful.")

# Get the list of available processes
processes <- processes()

# Define the process graph: load the Sentinel-2 L2A collection
datacube <- processes$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = aoi_openeo,
  temporal_extent = c(target_date, target_date) # Use the same date for start and end
)

# ==============================================================================
# 3. Create, Start, and Monitor OpenEO Job
# ==============================================================================
message("\n--- Starting OpenEO job asynchronously ---")

# Define job title and description for traceability on the backend
job_title <- paste0(study_site_name, "_Sen2_", gsub("-", "", target_date), "_Rjob")
job_description <- paste0("Getting Sentinel-2 L2A data for ", study_site_name, " on ", target_date)

message(paste0("Creating OpenEO job: '", job_title, "'"))
job <- create_job(
  graph = datacube,
  title = job_title,
  description = job_description
)
message(paste0("Job created with ID: ", job$id))

message(paste0("Starting OpenEO job: '", job$id, "'"))
start_job(job = job)

message("Job started. Monitoring job status (this may take a while)...")
# Loop to monitor the job until it is finished, has errored, or is canceled
while (TRUE) {
  job_status <- status(job)
  message(paste(Sys.time(), "- Job status:", job_status))
  if (job_status %in% c("finished", "error", "canceled")) {
    break
  }
  Sys.sleep(60) # Wait for 60 seconds before checking again
}

# ==============================================================================
# 4. Download and Finalize Results
# ==============================================================================
# Check final status and download if successful
if (job_status == "finished") {
  message("Job finished successfully. Downloading results...")
  
  # Ensure the output directory exists
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download results directly to the final output directory
  downloaded_files <- download_results(job = job, folder = output_dir)
  
  # Get the path to the actual downloaded raster file (usually the first one)
  temp_download_path <- downloaded_files[[1]]
  
  # --- Rename the downloaded file to a clean, standard name ---
  target_year <- year(as_date(target_date))
  new_filename <- paste0(study_site_name, "_Sen2_", target_year, ".tif")
  final_output_path <- file.path(output_dir, new_filename)
  
  # Move the downloaded file to its final, clean name
  file.rename(from = temp_download_path, to = final_output_path)
  
  message(paste0("\nSUCCESS: Sentinel-2 image saved to:\n", final_output_path))
  
} else {
  # If the job failed, print the logs for debugging
  message(paste0("OpenEO job failed with status '", job_status, "'."))
  message("You can view job logs in your OpenEO platform editor for job ID: ", job$id)
  logs <- logs(job)
  print(logs)
  stop("Aborting script due to job failure.")
}


# ==============================================================================
# Calculate Indices and Apply Model to a Local Sentinel-2 Image
# ------------------------------------------------------------------------------
# Purpose:
# This script loads a local Sentinel-2 image, calculates all derived spectral
# indices required by the model, applies the pre-trained rpart model, and
# saves the final classified map.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(terra)
library(rpart)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- USER-DEFINED PARAMETERS ---

# 1. Path to your downloaded Sentinel-2 image
s2_image_path <- "data/Sen2_workshop/Webbekomsbroek_Sen2_2025.tif" 

# 2. Path to your pre-trained R model
model_path <- "source/jussila_decisiontree.RData" 

# 3. Define the directory for the output classified raster
output_dir <- "output/classified_maps" 


# ==============================================================================
# 1️⃣ Load Input Data (Raster and Model)
# ==============================================================================
message("\n--- Loading input data ---")

# --- Load Sentinel-2 Raster ---
if (!file.exists(s2_image_path)) {
  stop("FATAL: Sentinel-2 image not found at: \n", s2_image_path)
}
s2_raster <- rast(s2_image_path)
message("Sentinel-2 image loaded successfully.")

# --- Load the pre-trained model ---
if (!file.exists(model_path)) {
  stop("FATAL: Model file not found at: ", model_path)
}
load(model_path, envir = .GlobalEnv) # Assumes model object is 'tree_jussila'
message("Decision tree model 'tree_jussila' loaded successfully.")


# ==============================================================================
# 2️⃣ Calculate Derived Indices from Original Bands
# ==============================================================================
message("\n--- Calculating derived spectral indices from ORIGINAL bands ---")

# --- 2.1 Prepare bands for index calculation ---
# We use the original s2_raster directly, with no scaling.
s2_unscaled <- s2_raster
names(s2_unscaled) <- tolower(names(s2_unscaled))

# --- 2.2 Calculate Indices Using Original Bands ---
ndvi <- (s2_unscaled$b8a - s2_unscaled$b04) / (s2_unscaled$b8a + s2_unscaled$b04)
names(ndvi) <- "ndvi"

ndwi_mf <- (s2_unscaled$b03 - s2_unscaled$b8a) / (s2_unscaled$b03 + s2_unscaled$b8a)
names(ndwi_mf) <- "ndwi_mf"

mndwi11 <- (s2_unscaled$b03 - s2_unscaled$b11) / (s2_unscaled$b03 + s2_unscaled$b11)
names(mndwi11) <- "mndwi11"

mndwi12 <- (s2_unscaled$b03 - s2_unscaled$b12) / (s2_unscaled$b03 + s2_unscaled$b12)
names(mndwi12) <- "mndwi12"

ndmi_gao11 <- (s2_unscaled$b8a - s2_unscaled$b11) / (s2_unscaled$b8a + s2_unscaled$b11)
names(ndmi_gao11) <- "ndmi_gao11"

# --- 2.3 Stack all potential predictors ---
# Combine the original unscaled bands and the new index layers
all_predictors_stack <- c(s2_unscaled, ndvi, ndwi_mf, mndwi11, mndwi12, ndmi_gao11)
message("All potential predictor layers have been calculated and stacked.")


# ==============================================================================
# 3️⃣ Prepare Raster and Apply Model
# ==============================================================================
message("\n--- Preparing final raster stack and applying model ---")

# Get the list of predictor names the model absolutely requires
model_predictor_names <- attr(tree_jussila$terms, "term.labels")
message("Model requires predictors: ", paste(model_predictor_names, collapse = ", "))

if (!all(model_predictor_names %in% names(all_predictors_stack))) {
  stop("FATAL: Not all required predictor layers could be found or calculated.")
}

# Subset the full stack to include only the required predictors
final_predictor_stack <- all_predictors_stack[[model_predictor_names]]
message("Final predictor stack created with the following layers:")
print(names(final_predictor_stack))

# Use terra::predict to apply the model
classified_map <- terra::predict(
  final_predictor_stack,
  tree_jussila,
  type = "class",
  na.rm = TRUE
)
message("Prediction complete.")


# ==============================================================================
# 4️⃣ Finalize and Save Classified Map
# ==============================================================================
message("\n--- Finalizing and saving the classified map ---")

levels(classified_map) <- data.frame(id = c(1, 2), category = c("dry", "water"))
color_map <- data.frame(value = c(1, 2), color = c("#dc5199", "#4cd2de"))
coltab(classified_map) <- color_map
message("Category labels and color map embedded in the final raster.")

# Add "_unscaled" to the output filename to distinguish it
output_filename <- paste0(tools::file_path_sans_ext(basename(s2_image_path)), "_jussila_classified_unscaled.tif")
final_output_path <- file.path(output_dir, output_filename)
dir.create(dirname(final_output_path), recursive = TRUE, showWarnings = FALSE)

writeRaster(
  classified_map,
  final_output_path,
  overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW"), datatype = "INT1U")
)

message(paste0("\nSUCCESS: Final classified map (from original bands) saved to:\n", final_output_path))

