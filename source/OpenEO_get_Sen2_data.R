# Load required libraries
library(sf)          # For handling shapefiles
library(openeo)      # For OpenEO platform interaction
library(ggplot2)     # Although not used directly, common for geospatial analysis
library(readr)       # For reading data
library(terra)       # For raster data handling
library(googledrive) # For Google Drive interaction (used via gdrive_utils)
library(dplyr)       # For data manipulation (used in gdrive_utils)
library(stringr)     # For strings


# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Study Site and Data Parameters ---
# Define the full study site name (for local folder names, job titles etc.)
#study_site_name <- "Webbekomsbroek"
#study_site_name <- "Webbekomsbroek2"
#study_site_name <- "Schulensmeer"
study_site_name <- "Kloosterbeemden"


# Define the abbreviation for the study site (used in shapefile names)
# e.g., "SM", "WB", "KB"
#study_site_abbreviation <- "WB" 
#study_site_abbreviation <- "WB2"  
#study_site_abbreviation <- "SM"  
study_site_abbreviation <- "KB"  

# Define the target year for data acquisition (used for lookup table & local folders)
#target_year <- 2020
#target_year <- 2021
target_year <- 2023
#target_year <- 2024

# Google Drive FILE ID for the Sentinel-2 date look-up table CSV.
# This is the direct ID of the 'sen2_dates.csv' file itself, not its folder.
sen2_lookup_file_id <- "1BoL8A0n-ot4i_tr2_0P3HWQkwM4HlMk_" # <--- IMPORTANT: SET THIS FILE ID

# Name of the Sentinel-2 date look-up table file (for local saving)
sen2_lookup_filename <- "sen2_dates.csv"

# Google Drive ID for the *single folder* containing ALL 'Extent_STUDYSITENAME.shp' files.
extent_gdrive_folder_id <- "1rGYakNjSw9qvbjCNTA7KlDR23crywWAa" # <--- IMPORTANT: SET THIS ID

# Construct the exact filename for the extent shapefile based on study_site_name
# NAMING CONVENTION: "Extent_FullStudySiteName.shp"
extent_shp_filename <- paste0("Extent_", str_replace_all(study_site_name, " ", ""), ".shp") 
# Note: Replaced spaces in study_site_name for filename, e.g., "Webbekoms Broek" -> "WebbekomsBroek"
# Adjust this if your actual Google Drive filenames handle spaces differently or use abbreviations.


# Base directory for storing downloaded Sentinel-2 data
base_output_dir_sen2 <- "data/Sen2"

# Google Drive folder ID for uploading Sentinel-2 results
sen2_gdrive_output_folder_id <- "1wQ00Kdzbjsv3iRbTd_9TOTHlvGpvvgFG" 


# --- OpenEO Backend Connection ---
openeo_backend_url <- "https://openeo.dataspace.copernicus.eu"


# ==============================================================================
# 0️⃣ Load Google Drive Utility Functions
# ==============================================================================
# Make sure gdrive_utils.R is in the 'source' subdirectory or provide the full path
source("source/gdrive_utils.R")


# ==============================================================================
# 0.5 Acquire and Process Sentinel-2 Date Look-up Table
# ==============================================================================

# Local subfolder for the lookup table download (creates 'data/lookup_tables')
local_lookup_dir <- file.path("data", "lookup_tables") # Directly to data/lookup_tables

# Ensure the local directory exists
if (!dir.exists(local_lookup_dir)) {
  message(paste0("Creating local directory for lookup table: ", local_lookup_dir))
  dir.create(local_lookup_dir, recursive = TRUE, showWarnings = FALSE)
}

# Construct the local path to where the lookup table will be saved
local_sen2_lookup_table_path <- file.path(local_lookup_dir, sen2_lookup_filename)

# Direct download of the lookup table using its file ID
message(paste0("Attempting to download lookup table '", sen2_lookup_filename, "' from Google Drive file ID: ", sen2_lookup_file_id))

# Authenticate Google Drive before the direct download if not already done
auth_success <- authenticate_gdrive()
if (!auth_success) {
  stop("Google Drive authentication failed. Cannot download lookup table.")
}

tryCatch({
  googledrive::drive_download(
    file = as_id(sen2_lookup_file_id),
    path = local_sen2_lookup_table_path,
    overwrite = TRUE # This ensures any existing local file is replaced
  )
  message(paste0("Successfully downloaded lookup table to: ", local_sen2_lookup_table_path))
}, error = function(e) {
  stop(paste0("Failed to download lookup table from Google Drive. Error: ", conditionMessage(e)))
})


if (!file.exists(local_sen2_lookup_table_path)) {
  stop(paste0("Downloaded look-up table not found at: ", local_sen2_lookup_table_path, ". Download may have failed."))
}

message(paste0("Loading Sentinel-2 date look-up table from local path: ", local_sen2_lookup_table_path))
sen2_dates_lookup <- readr::read_csv(local_sen2_lookup_table_path)

# Filter the lookup table for the specific study site and year
target_date_info <- sen2_dates_lookup %>%
  filter(
    `study site` == study_site_abbreviation,
    year == target_year
  )

if (nrow(target_date_info) == 0) {
  stop(paste0("No 'Sen2-date' found for study site '", study_site_abbreviation, "' and year '", target_year, "' in the lookup table. Please check your lookup table or inputs."))
} else if (nrow(target_date_info) > 1) {
  warning(paste0("Multiple 'Sen2-date' entries found for study site '", study_site_abbreviation, "' and year '", target_year, "'. Using the first one."))
  temporal_date <- as.character(target_date_info$`Sen2-date`[1])
} else {
  temporal_date <- as.character(target_date_info$`Sen2-date`[1])
}

# Define the temporal extent using the retrieved date
temporal_start_date <- temporal_date
temporal_end_date <- temporal_date # For a single day, start and end dates are the same

message(paste0("Sentinel-2 acquisition date determined from lookup table: ", temporal_date))


# ==============================================================================
# 1. Acquire Boundary Shapefile from Google Drive
# ==============================================================================

message("Authenticating Google Drive for boundary shapefile acquisition...")
# Authentication already happened for lookup table download.

# Local folder to store the downloaded extent shapefile components
# This will be 'data/Extent_studiegebieden'
local_extent_dir <- file.path("data", "Extent_studiegebieden")


# Ensure the local directory for extent shapefiles exists
if (!dir.exists(local_extent_dir)) {
  message(paste0("Creating local directory for extent shapefiles: ", local_extent_dir))
  dir.create(local_extent_dir, recursive = TRUE, showWarnings = FALSE)
}


# Download the extent shapefile components using the utility function
message(paste0("Downloading extent shapefile '", extent_shp_filename, "' for ", study_site_name, "..."))
download_shapefile_components(
  gdrive_folder_id = extent_gdrive_folder_id, # Use the new extent folder ID
  target_shp_filename = extent_shp_filename,   # The specific filename within that folder
  base_local_cache_dir = local_extent_dir,     # This is now the target directory for this shapefile
  local_target_subfolder = "."                   # Download directly into local_extent_dir
)

# Read the downloaded .shp file for the boundary
boundary_shp_file_path <- list.files(
  local_extent_dir,
  pattern = paste0("^", tools::file_path_sans_ext(extent_shp_filename), "\\.shp$"), # Use extent_shp_filename for pattern
  full.names = TRUE,
  recursive = FALSE
)[1]

if (is.na(boundary_shp_file_path) || !file.exists(boundary_shp_file_path)) {
  stop(paste0("Boundary shapefile '", boundary_shp_file_path, "' not found in '", local_extent_dir, "'. Ensure it was downloaded correctly."))
}
aoi_polygon <- st_read(boundary_shp_file_path)
message(paste0("Successfully loaded boundary shapefile from: ", boundary_shp_file_path))
message("Boundary shapefile original CRS: ", st_crs(aoi_polygon)$input)


# ------------------------------------------------------------------------------
# 2 Transform Boundary Shapefile and Define OpenEO Bounding Box
# ------------------------------------------------------------------------------

# Convert boundary polygon to WGS 84 (EPSG:4326) for OpenEO processing
aoi_polygon_wgs84 <- st_transform(aoi_polygon, crs = 4326)
message("Boundary shapefile transformed to CRS: ", st_crs(aoi_polygon_wgs84)$input)

# Define bounding box from transformed boundary polygon
aoi_bbox_wgs84 <- st_bbox(aoi_polygon_wgs84)

aoi_openeo <- list(
  west = as.numeric(aoi_bbox_wgs84["xmin"]),
  east = as.numeric(aoi_bbox_wgs84["xmax"]),
  south = as.numeric(aoi_bbox_wgs84["ymin"]),
  north = as.numeric(aoi_bbox_wgs84["ymax"])
)
message("Defined OpenEO AoI bounding box (WGS84):")
print(aoi_openeo)


# ------------------------------------------------------------------------------
# 3 Connect to OpenEO Backend and Authenticate
# ------------------------------------------------------------------------------
message(paste0("Connecting to OpenEO backend: ", openeo_backend_url))
con <- tryCatch({
  connect(openeo_backend_url)
}, error = function(e) {
  stop(paste0("Failed to connect to OpenEO backend: ", openeo_backend_url, ". Error: ", e$message))
})
message("Connection successful. Attempting login...")

tryCatch({
  login()
  message("OpenEO login successful.")
}, error = function(e) {
  stop(paste0("Failed to log in to OpenEO. Error: ", e$message, ". Please check your credentials or API keys."))
})


# ------------------------------------------------------------------------------
# 4 Load Sentinel-2 Data
# ------------------------------------------------------------------------------
message("Defining Sentinel-2 datacube process graph...")
processes <- processes()

datacube <- processes$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = aoi_openeo, # Use the derived AoI from the boundary shapefile
  temporal_extent = c(temporal_start_date, temporal_end_date) # Dates from lookup table
)

# Define Output Format for OpenEO Job
openeo_output_format <- "GTiff" # Or "NetCDF", "JPEG2000", "PNG" etc.

# ------------------------------------------------------------------------------
# 5 Create and Start OpenEO Job
# ------------------------------------------------------------------------------
job_title <- paste0(study_site_name, "_Sen2_", format(as.Date(temporal_start_date), "%Y%m%d"), "_Rjob")
job_description <- paste0("Getting Sentinel-2 L2A data for ", study_site_name, " on ", temporal_start_date, " using '", extent_shp_filename, "' boundary.")

message(paste0("Creating OpenEO job: '", job_title, "'"))
job <- tryCatch({
  create_job(
    graph = datacube,
    title = job_title,
    description = job_description,
    job_options = list(output_format = openeo_output_format) # Specify output format
  )
}, error = function(e) {
  stop(paste0("Failed to create OpenEO job. Error: ", e$message))
})
message(paste0("Job created with ID: ", job$id))

message(paste0("Starting OpenEO job: '", job$id, "'"))
start_job_status <- tryCatch({
  start_job(job = job)
  TRUE
}, error = function(e) {
  warning(paste0("Failed to start OpenEO job. Error: ", e$message))
  FALSE
})

if (!start_job_status) {
  stop("OpenEO job failed to start. Aborting data download.")
}

message("Job started. Monitoring job status (this may take a while)...")

# ------------------------------------------------------------------------------
# 6 Download OpenEO data
# ------------------------------------------------------------------------------

# Construct the local folder path for results
# This will be directly 'data/Sen2'
output_folder_path <- base_output_dir_sen2 

# Ensure the local output directory exists
if (!dir.exists(output_folder_path)) {
  message(paste0("Creating output directory: ", output_folder_path))
  dir.create(output_folder_path, recursive = TRUE, showWarnings = FALSE)
}

message(paste0("Downloading OpenEO job results to: ", output_folder_path))

# Download the results and capture the actual path returned by openeo
download_results_path <- tryCatch({
  download_results(job = job, folder = output_folder_path)
}, error = function(e) {
  warning(paste0("Failed to download OpenEO job results. Error: ", e$message))
  return(NULL) # Return NULL if download failed
})

if (is.null(download_results_path)) {
  stop("OpenEO data download failed. Cannot rename or upload.")
}

# The actual path to the downloaded file. Assume it's a single file.
actual_downloaded_path <- download_results_path[[1]]


# Determine the file extension based on the chosen output format
file_extension <- switch(openeo_output_format,
                         "GTiff" = ".tif",
                         "NetCDF" = ".nc",
                         "JPEG2000" = ".jp2",
                         "PNG" = ".png",
                         ".unknown" # Default if format not recognized
)

# Construct the new desired local filename (e.g., "Webbekoms Broek_Sen2_2020.tif")
new_local_filename <- paste0(
  study_site_name, "_Sen2_",
  target_year, # Use target_year directly
  file_extension
)

# Construct the full path for the renamed file
new_full_local_path <- file.path(output_folder_path, new_local_filename)

# Rename the downloaded file only if its current name is different from the desired name
# and confirm the file exists before renaming.
if (file.exists(actual_downloaded_path) && actual_downloaded_path != new_full_local_path) {
  message(paste0("Renaming downloaded file from '", basename(actual_downloaded_path), "' to '", new_local_filename, "'"))
  file.rename(from = actual_downloaded_path, to = new_full_local_path)
} else if (file.exists(new_full_local_path)) {
  message(paste0("Downloaded file is already named '", new_local_filename, "' or was previously renamed."))
} else {
  stop(paste0("Expected downloaded file at '", actual_downloaded_path, "' not found for renaming."))
}


# Upload the renamed file to Google Drive
message(paste0("Uploading '", new_local_filename, "' to Google Drive folder ID: ", sen2_gdrive_output_folder_id))
upload_status <- tryCatch({
  googledrive::drive_upload(
    media = new_full_local_path, # Use the final renamed local path
    path = as_id(sen2_gdrive_output_folder_id),
    overwrite = TRUE # Overwrite if a file with the same name already exists in GDrive
  )
  TRUE
}, error = function(e) {
  warning(paste0("Failed to upload '", new_local_filename, "' to Google Drive. Error: ", e$message))
  FALSE
})

if (upload_status) {
  message("Sentinel-2 data download, rename, and upload complete.")
} else {
  message("Sentinel-2 data upload failed.")
}

