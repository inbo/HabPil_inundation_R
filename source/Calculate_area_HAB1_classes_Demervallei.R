# ==============================================================================
# 📊  Ordered Bar Plots for HAB1 area (ha) in Grassland & Wetland in Demervallei
# ==============================================================================

# Load required libraries
library(sf)          # For handling shapefiles
library(dplyr)       # For data manipulation
library(stringr)     # For working with strings
library(ggplot2)     # For visualization
library(googledrive) # For getting and saving files from/to Google Drive

# ==========================================
# 1️⃣ Load the Shapefile of the BWK Demervallei
# ==========================================

# Save a local copy of the needed files

# Google Drive folder ID for the correct shapefile (Dijlevallei)
drive_folder_id <- "1SthW-UKPD_gmU56Kr8yfqokSRq775_Po"

# Local folder to store the shapefile
site_name <- "Demervallei"
local_dir <- file.path("data/BWK 2023", site_name)

# Create the local folder if it doesn't exist
dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)

# List files in the Drive subfolder
drive_files <- drive_ls(path = as_id(drive_folder_id))

# Download only missing files
for (i in seq_len(nrow(drive_files))) {
  local_path <- file.path(local_dir, drive_files$name[i])
  if (!file.exists(local_path)) {
    message(paste("Downloading", drive_files$name[i]))
    drive_download(as_id(drive_files$id[i]), path = local_path, overwrite = TRUE)
  } else {
    message(paste("Already exists:", drive_files$name[i]))
  }
}

# Find and read the .shp file
shp_file <- list.files(local_dir, pattern = "\\.shp$", full.names = TRUE)[1]
demervallei <- st_read(shp_file)

# ==========================================
# 2️⃣ Filter Data for Grassland & Wetland
# ==========================================

# Separate grassland and wetland using EENH1 first letter
grassland_data <- demervallei %>%
  st_drop_geometry() %>%
  filter(str_to_lower(str_sub(EENH1, 1, 1)) == "h")  # Grassland (EENH1 starts 
    # with 'h')

wetland_data <- demervallei %>%
  st_drop_geometry() %>%
  filter(str_to_lower(str_sub(EENH1, 1, 1)) == "m")  # Wetland (EENH1 starts 
    # with 'm')

# ==========================================
# 3️⃣ Calculate Total Area per HAB1
# ==========================================

# Compute total area per HAB1 for Grassland
grassland_area <- grassland_data %>%
  group_by(HAB1) %>%
  summarise(Total_Area_ha = sum(OPPERVL, na.rm = TRUE) / 10000, .groups = 
  "drop")  # Convert m² to ha

# Compute total area per HAB1 for Wetland
wetland_area <- wetland_data %>%
  group_by(HAB1) %>%
  summarise(Total_Area_ha = sum(OPPERVL, na.rm = TRUE) / 10000, .groups = 
  "drop")  # Convert m² to ha

# ==========================================
# 4️⃣ Create Ordered Bar Plots
# ==========================================

# Function to create an ordered bar plot
plot_bar_chart <- function(data, title) {
  ggplot(data, aes(x = reorder(HAB1, Total_Area_ha), y = Total_Area_ha, fill = 
  Total_Area_ha)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = paste0(round(Total_Area_ha, 1), " ha")), hjust = 
    -0.1, size = 4) +  # Add hectare labels
    scale_fill_viridis_c() +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),      # removes all grid lines
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_line(color = "black"),                   
      axis.line = element_line(color = "black")) +                     
    labs(title = title, x = "HAB1 Class", y = "Total Area (ha)") +
    coord_flip()  # Flip axes for better readability
}

# Plot for Grassland
grassland_plot <- plot_bar_chart(grassland_area, "Total Area per HAB1 
(Grassland)")

# Plot for Wetland
wetland_plot <- plot_bar_chart(wetland_area, "Total Area per HAB1 (Wetland)")

# ==========================================
# 5️⃣ Display the Plots
# ==========================================

print(grassland_plot)
print(wetland_plot)

# ==========================================  
# 6️⃣ Save the Plos Locally
# ==========================================

# Save the plots as PNG files
ggsave("output/BWK_exploration/Calculate_area_HAB1/Demervallei_HAB1_Area_h.png", grassland_plot, 
       width = 10, height = 6)
ggsave("output/BWK_exploration/Calculate_area_HAB1/Demervallei_HAB1_Area_m.png", wetland_plot, 
       width = 10, height = 6)


# ==========================================
# 7️⃣ Upload the Plots to Google Drive
# ==========================================

# Set your Google Drive folder ID
target_folder_id <- "1CpH2prLEeY4Jo0stJKmSdQutZrBPFsZJ"  # Replace if needed

# Upload plots to Google Drive
drive_upload(
  media = "output/BWK_exploration/Calculate_area_HAB1/Demervallei_HAB1_Area_h.png",
  path = as_id(target_folder_id),
  overwrite = TRUE
)

drive_upload(
  media = "output/BWK_exploration/Calculate_area_HAB1/Demervallei_HAB1_Area_m.png",
  path = as_id(target_folder_id),
  overwrite = TRUE
)

