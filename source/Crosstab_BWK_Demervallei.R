# Load necessary libraries
library(sf)          # For handling shapefiles
library(dplyr)       # For data manipulation
library(stringr)     # For string operations
library(ggplot2)     # For plotting heatmaps
library(tidyr)       # For reshaping data
library(scales)      # For formatting percentages
library(googledrive) # For getting and saving files from/to Google Drive

# ==========================
# 1️⃣ Load the Shapefile of the BWK Demervallei
# ==========================

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

# ==========================
# 2️⃣ Extract First Letter of EENH1
# ==========================

# Create a new column with the first letter of EENH1 (in lowercase)
demervallei <- demervallei %>%
  mutate(EENH1_Group = str_to_lower(str_sub(EENH1, 1, 1)))  # Ensure lowercase

# ==========================
# 3️⃣ Cross-Tabulate EENH1 & HAB1 Based on Area
# ==========================

# Perform cross-tabulation, summing the polygon area (OPPERVL)
crosstab <- demervallei %>%
  st_drop_geometry() %>%  # Remove geometry for tabular operations
  group_by(EENH1, HAB1) %>%
  summarise(Total_Area = sum(OPPERVL, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = HAB1, values_from = Total_Area, values_fill = 0)  
    # Convert to wide format

# Normalize the values to **relative proportions per HAB1**
crosstab_long <- demervallei %>%
  st_drop_geometry() %>%
  group_by(EENH1, HAB1) %>%
  summarise(Total_Area = sum(OPPERVL, na.rm = TRUE), .groups = "drop") %>%
  group_by(HAB1) %>%
  mutate(Relative_Area = Total_Area / sum(Total_Area, na.rm = TRUE)) %>%
  ungroup()

# ==========================
# 4️⃣ Generate Heatmaps for EENH1 Starting with 'h' & 'm'
# ==========================

# Set target folder in Drive
target_folder_id <- "1AhkgIEDFaVP7KGcbBxA6lHT8hdAo5hiM" 

# Define the output folder
output_dir <- "output/BWK_exploration"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create the folder if it 
  # doesn't exist
}

# Only keep "h" and "m"
selected_letters <- c("h", "m")
filtered_data <- filter(crosstab_long, str_sub(EENH1, 1, 1) %in% 
                          selected_letters)

# Loop over the selected first letters to create heatmaps
for (letter in selected_letters) {
  
  # Filter data for the current letter
  plot_data <- filter(filtered_data, str_sub(EENH1, 1, 1) == letter)
  
  # Identify HAB1 categories where all values are 0%
  hab1_sums <- plot_data %>%
    group_by(HAB1) %>%
    summarise(Total = sum(Relative_Area, na.rm = TRUE), .groups = "drop")
  
  # Keep only HAB1 values where at least one entry is >0
  non_zero_hab1 <- hab1_sums %>% filter(Total > 0) %>% pull(HAB1)
  
  # Filter original dataset to keep only non-zero HAB1 values
  plot_data <- plot_data %>% filter(HAB1 %in% non_zero_hab1)
  
  # Create a combined label with both percentage and area values
  plot_data <- plot_data %>%
    mutate(Label = paste0(scales::percent(Relative_Area, accuracy = 0.1),  
                          "\n",  # New line for better readability
                          format(round(Total_Area/10000, 1), big.mark = " "), 
                          " ha"))  # Format area
  
  # Calculate number of rows and columns for the current letter
  num_rows <- length(unique(plot_data$EENH1))
  num_cols <- length(unique(plot_data$HAB1))
  
  # Adjust text size dynamically
  text_size <- case_when(
    num_rows > 20 | num_cols > 20 ~ 2,  # Very large grid → smallest text
    num_rows > 15 | num_cols > 15 ~ 2.5,
    num_rows > 10 | num_cols > 10 ~ 3,
    num_rows > 5 | num_cols > 5 ~ 4,
    TRUE ~ 5  # Very small grid → largest text
  )
  
  # Adjust figure width and height dynamically
  plot_width <- max(10, num_cols * 0.5)  # Increase width based on the number of columns
  plot_height <- max(8, num_rows * 0.4)  # Increase height based on the number of rows
  
  # Generate the heatmap with optimized text size
  p <- ggplot(plot_data, aes(x = HAB1, y = EENH1, fill = Relative_Area)) +
    geom_tile(color = "white") +  # Add white gridlines for readability
    geom_text(aes(label = Label), size = text_size) +  # Adjust text size dynamically
    scale_fill_viridis_c(option = "plasma", na.value = "white", labels = scales::percent) +
    theme_minimal(base_size = 12) +  # Keep a clean look
    theme(panel.background = element_rect(fill = "white", color = NA),  # Set white background
          plot.background = element_rect(fill = "white", color = NA),   # Set outer white background
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10)) +
    labs(title = paste("Relative Proportion of EENH1 within Each HAB1 (", letter, ")", sep = ""),
         x = "HAB1", y = "EENH1", fill = "Relative Area")
  
  # Save the heatmap with dynamic figure size
  if (nrow(plot_data) > 0) {
    ggsave(filename = paste0(output_dir, "/Crosstab_BWK/Demervallei_heatmap_", letter, ".png"),
           plot = p, width = plot_width, height = plot_height, dpi = 300)
    # Upload file
    drive_upload(
      media = paste0(output_dir, "/Crosstab_BWK/Demervallei_heatmap_", letter, ".png"),
      path = as_id(target_folder_id),
      overwrite =  TRUE
    )
  }
}



# ==========================
# 5️⃣ Save Separate Shapefiles for 'h' & 'm'
# ==========================
# Set target folder in Drive
target_folder_id2 <- "1CNbyv0XRaMYUYMKfjfkpJ8nZYa-Xsd_F" 

# Function to save filtered data as .gpkg
save_filtered_geopackage <- function(letter) {
  filtered_data <- filter(demervallei, EENH1_Group == letter)
  
  if (nrow(filtered_data) > 0) {
    gpkg_path <- file.path(output_dir, paste0("BWK_per_class/Demervallei_BWK2023_", letter, ".gpkg"))
    
    # Save as GeoPackage
    st_write(filtered_data, gpkg_path, delete_dsn = TRUE)
    
    # Upload to Google Drive
    googledrive::drive_upload(
      media = gpkg_path,
      path = as_id(target_folder_id2),
      overwrite = TRUE
    )
    
    message(paste("Saved and uploaded:", gpkg_path))
  } else {
    message(paste("No features found for EENH1_Group =", letter))
  }
}

# Save and upload for 'h' and 'm'
save_filtered_geopackage("h")
save_filtered_geopackage("m")

