# ==============================================================================
# Calculate All Study Site Centroids
# ==============================================================================
# Description:
# This script reads a lookup table from Google Drive to identify all unique
# study site abbreviations. It then iterates through each site, uses an internal
# mapping to find the full site name, downloads the respective boundary
# shapefile, and calculates the geographic centroid. The final output is a
# single CSV file with coordinates for every study site.
#
# ==============================================================================

# Load required libraries
library(sf)          # For handling shapefiles and geospatial calculations
library(readr)       # For reading the CSV lookup table
library(googledrive) # For Google Drive interaction
library(dplyr)       # For data manipulation
library(stringr)     # For string manipulation

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Site Name Mapping ---
# Provides the full name for each abbreviation found in the lookup table.
# This is necessary to find the correct 'Extent_*.shp' file.
# ADD OR EDIT SITES HERE AS NEEDED.
site_name_mapping <- list(
  "WB" = "Webbekomsbroek",
  "SM" = "Schulensmeer",
  "KB" = "Kloosterbeemden",
  "WB2" = "Webbekomsbroek2"
)

# --- Google Drive IDs ---
# GDrive FILE ID for the Sentinel-2 date look-up table CSV.
# This table must contain a 'study site' column with the abbreviations.
sen2_lookup_file_id <- "1BoL8A0n-ot4i_tr2_0P3HWQkwM4HlMk_"

# GDrive FOLDER ID for the folder containing ALL 'Extent_*.shp' files.
extent_gdrive_folder_id <- "1rGYakNjSw9qvbjCNTA7KlDR23crywWAa"

# --- Local File Name ---
sen2_lookup_filename <- "sen2_dates.csv"

# ==============================================================================
# 0️⃣ Load Google Drive Utility Functions
# ==============================================================================
# Ensure gdrive_utils.R is in the 'source' subdirectory or provide the full path.
source("source/gdrive_utils.R")

# Authenticate with Google Drive early
message("Authenticating with Google Drive...")
if (!authenticate_gdrive()) {
  stop("Google Drive authentication failed. Please check your credentials.")
}

# ==============================================================================
# 1️⃣ Acquire Look-up Table and Identify Unique Study Sites
# ==============================================================================
local_lookup_dir <- file.path("data", "lookup_tables")
if (!dir.exists(local_lookup_dir)) {
  dir.create(local_lookup_dir, recursive = TRUE)
}
local_sen2_lookup_table_path <- file.path(local_lookup_dir, sen2_lookup_filename)

# Download the lookup table if it doesn't exist locally
if (!file.exists(local_sen2_lookup_table_path)) {
  message("Downloading lookup table from Google Drive...")
  googledrive::drive_download(
    file = as_id(sen2_lookup_file_id),
    path = local_sen2_lookup_table_path,
    overwrite = TRUE
  )
} else {
  message("Lookup table already exists locally.")
}

# Read the lookup table and find all unique study site abbreviations
sen2_dates_lookup <- readr::read_csv(local_sen2_lookup_table_path, show_col_types = FALSE)

unique_site_abbrs <- sen2_dates_lookup %>%
  distinct(`study site`) %>%
  pull(`study site`)

if (length(unique_site_abbrs) == 0) {
  stop("No unique sites found in the 'study site' column of the lookup table.")
}

message(paste0("Found ", length(unique_site_abbrs), " unique study site abbreviations to process: ", paste(unique_site_abbrs, collapse=", ")))

# ==============================================================================
# 2️⃣ Download all Shapefiles and then Calculate Centroids
# ==============================================================================
# Create a single, shared directory for all extent shapefiles
local_extent_dir <- file.path("data", "Extent_studiegebieden")
if (!dir.exists(local_extent_dir)) {
  dir.create(local_extent_dir, recursive = TRUE)
}

# --- Pre-download all files from the Google Drive folder ---
message("\n--- Pre-downloading all files from Google Drive folder ---")
tryCatch({
  all_gdrive_files <- googledrive::drive_ls(as_id(extent_gdrive_folder_id))
  
  if (nrow(all_gdrive_files) == 0) {
    stop("No files found in the specified Google Drive folder.")
  }
  
  message(paste0("Found ", nrow(all_gdrive_files), " files to download."))
  
  # Loop and download each file individually
  for (i in 1:nrow(all_gdrive_files)) {
    target_file <- all_gdrive_files[i,]
    local_path <- file.path(local_extent_dir, target_file$name)
    message(paste0("Downloading '", target_file$name, "'..."))
    googledrive::drive_download(file = target_file, path = local_path, overwrite = TRUE)
  }
  
  message("--- All files downloaded successfully. ---")
  
}, error = function(e) {
  stop(paste0("Failed to download files from Google Drive. Error: ", e$message))
})

# --- Loop through sites and process the local files ---
results_list <- list()

for (current_site_abbr in unique_site_abbrs) {
  current_site_name <- site_name_mapping[[current_site_abbr]]
  
  if (is.null(current_site_name)) {
    warning(paste0("Abbreviation '", current_site_abbr, "' not in 'site_name_mapping'. Skipping."))
    next
  }
  
  message(paste0("\n--- Processing: ", current_site_name, " (", current_site_abbr, ") ---"))
  
  # --- Locate the locally downloaded Boundary Shapefile ---
  extent_shp_filename <- paste0("Extent_", str_replace_all(current_site_name, " ", ""), ".shp")
  boundary_shp_file_path <- list.files(
    local_extent_dir,
    pattern = paste0("^", tools::file_path_sans_ext(extent_shp_filename), "\\.shp$"),
    full.names = TRUE
  )[1]
  
  if (is.na(boundary_shp_file_path) || !file.exists(boundary_shp_file_path)) {
    warning(paste0("Local shapefile not found for '", current_site_name, "'. Skipping."))
    next
  }
  
  # --- Calculate Bounding Box Centroid (Lon/Lat) ---
  message("Calculating bounding box centroid...")
  aoi_polygon <- st_read(boundary_shp_file_path, quiet = TRUE)
  
  # Get the bounding box in the *original* projected CRS
  bbox_projected <- st_bbox(aoi_polygon)
  
  # Convert the bounding box to a polygon and find its centroid in the projected CRS
  centroid_projected <- st_centroid(st_as_sfc(bbox_projected))
  
  # Now, transform the accurate centroid point to WGS 84
  centroid_wgs84 <- st_transform(centroid_projected, crs = 4326)
  
  # Extract the final coordinates
  coords <- st_coordinates(centroid_wgs84)
  longitude <- coords[1, "X"]
  latitude <- coords[1, "Y"]
  
  # Store the result in the list
  results_list[[current_site_name]] <- data.frame(
    study_site = current_site_name,
    abbreviation = current_site_abbr,
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )
  message("Bounding box centroid calculation complete.")
}

# ==============================================================================
# 3️⃣ Consolidate and Save All Results
# ==============================================================================
if (length(results_list) == 0) {
  stop("Processing complete, but no centroids were successfully calculated.")
}

# Combine the list of individual data frames into a single data frame
all_centroids <- bind_rows(results_list)

# --- Print to Console ---
message("\n--------------------------------------------------")
message("          All Study Site Centroids")
message("--------------------------------------------------")
print(all_centroids)
message("--------------------------------------------------")

# --- Save to CSV File ---
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
