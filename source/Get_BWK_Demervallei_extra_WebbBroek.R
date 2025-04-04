Load necessary libraries
library(sf)          # For handling shapefiles
library(dplyr)       # For data manipulation
library(stringr)     # For string operations
library(googledrive) # For getting and saving files from/to Google Drive

# ==========================
# 1️⃣ Load the Shapefile of  extra Webbekoms Broek
# ==========================

# Get Google Drive folder for extra data
drive_folder_id <- "1Pwj4hjBBGP9q8LBl6TGaoPrLdkKKAQGZ"  # 

# Local folder to store the shapefile
local_dir <- "data/BWK 2023/Demervallei/extra_WebbBroek"
dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)

# Get files from Google Drive
drive_files <- drive_ls(path = as_id(drive_folder_id))

# Download missing files
for (i in seq_len(nrow(drive_files))) {
  local_path <- file.path(local_dir, drive_files$name[i])
  if (!file.exists(local_path)) {
    message(paste("Downloading", drive_files$name[i]))
    drive_download(as_id(drive_files$id[i]), path = local_path, overwrite = TRUE)
  } else {
    message(paste("Already exists:", drive_files$name[i]))
  }
}

# Manually choose the correct shapefile if needed
shp_file <- list.files(local_dir, pattern = "\\.shp$", full.names = TRUE)[1]
extra_WebbBroek <- st_read(shp_file)

# ==========================
# 🍃 Separate Grasslands & Wetlands
# ==========================

extra_WebbBroek<- extra_WebbBroek %>%
  mutate(EENH1_Group = str_to_lower(str_sub(EENH1, 1, 1)))

# ==========================
# 💾 Save Grassland & Wetland Shapefiles from New Area
# ==========================

# Set Google Drive target folder 
target_folder_id_sub <- "1CNbyv0XRaMYUYMKfjfkpJ8nZYa-Xsd_F"  

# Define output directory
output_dir <- "output/BWK_exploration/"
dir.create(file.path(output_dir, "BWK_per_class"), recursive = TRUE, showWarnings = FALSE)

# Generalized save function
save_filtered_geopackage <- function(letter) {
  filtered_data <- filter(extra_WebbBroek, EENH1_Group == letter)
  
  if (nrow(filtered_data) > 0) {
    gpkg_path <- file.path(output_dir, "BWK_per_class", paste0("Demervallei_BWK2023_extraWebbBroek_", letter, ".gpkg"))
    
    # Save as GeoPackage
    st_write(filtered_data, gpkg_path, delete_dsn = TRUE)
    
    # Upload to subfolder in Drive
    googledrive::drive_upload(
      media = gpkg_path,
      path = as_id(target_folder_id_sub),
      overwrite = TRUE
    )
    
    message(paste("Saved and uploaded:", gpkg_path))
  } else {
    message(paste("No features found for EENH1_Group =", letter))
  }
}

# Save and upload grasslands ('g') and wetlands ('m') for the new area
save_filtered_geopackage("h")
save_filtered_geopackage("m")
