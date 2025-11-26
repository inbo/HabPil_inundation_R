# Load necessary libraries
library(sf)          # For handling shapefiles
library(dplyr)       # For data manipulation
library(stringr)     # For string operations
library(ggplot2)     # For plotting heatmaps
library(tidyr)       # For reshaping data
library(scales)      # For formatting percentages
library(googledrive) # For getting and saving files from/to Google Drive

# ==========================
# 0️⃣ Configuration and Setup
# ==========================

# Define the study site name here
# Change this variable to analyze a different site
study_site_name <- "Demervallei"
#study_site_name <- "Dijlevallei"

# IMPORTANT: Provide the DIRECT Google Drive ID for the specific study site's folder.
# You will need to find this ID manually from your Google Drive for each study site.
# The path on Google Drive is '2024_Biodiversa_habitatpilot/WP2_3/Inundation/HabPil_inundation_R data/Data/BWK 2023'
site_specific_gdrive_id <- "1U50pjg3KabLn1cEOxtTlBwp1g6IHxK88" # for Demervallei
#site_specific_gdrive_id <- "1UGoFGhPSMIB9l5i98OWtW-51nhL9e62G" # for Dijlevallei

# Base local directory for all cached data
base_cache_directory <- "data/BWK 2023"

# Base name for the shapefile within each site's folder
# Assumes the .shp file is named like "SITE_NAME_BWK2023.shp"
shp_base_filename <- paste0(study_site_name, "_BWK2023")
target_shp_filename <- paste0(shp_base_filename, ".shp")


# ==========================
# 0️⃣ Load Google Drive Utility Functions
# ==========================

source("source/gdrive_utils.R")

# ==========================
# 1️⃣ Authenticate and Acquire Data for the Specific Study Site
# ==========================

# First, ensure Google Drive authentication is done
# The authenticate_gdrive() function is in gdrive_utils.R
auth_success <- authenticate_gdrive()
if (!auth_success) {
  stop("Google Drive authentication failed. Cannot proceed with data acquisition.")
}

# Configure the dataset to download for the current study site
# Now, we directly use the 'site_specific_gdrive_id' provided above.
datasets_to_acquire <- list(
  list(
    name = paste0(study_site_name, "_BWK2023"),
    gdrive_id = site_specific_gdrive_id, # <--- Direct ID provided here
    target_filename = target_shp_filename,
    local_subfolder = study_site_name # Explicitly set the local subfolder name for *input* data
  )
)

# Perform data acquisition using the utility function
downloaded_paths <- perform_project_data_acquisition(
  datasets_config = datasets_to_acquire,
  base_cache_dir = base_cache_directory
)

# Check if the dataset for the current study site was successfully downloaded
site_local_dir <- downloaded_paths[[paste0(study_site_name, "_BWK2023")]]

if (is.null(site_local_dir) || !dir.exists(site_local_dir)) {
  stop(paste0("Failed to acquire ", study_site_name, "_BWK2023 dataset. Aborting script."))
} else {
  message(paste0(study_site_name, "_BWK2023 dataset available at: ", site_local_dir))
}

# Read the .shp file from the acquired local directory
shp_file <- list.files(
  site_local_dir,
  pattern = paste0("^", shp_base_filename, "\\.shp$"), # Exact match for the .shp file
  full.names = TRUE,
  recursive = FALSE # Only look in the top level of the subfolder
)[1]

if (is.na(shp_file) || !file.exists(shp_file)) {
  stop(paste0("Main shapefile '", target_shp_filename, "' not found in '", site_local_dir, "'. Ensure it was downloaded correctly.", sep=""))
}
# Assign the loaded shapefile to a dynamically named object
# Using 'site_data' for consistency, but you could use assign(tolower(study_site_name), st_read(shp_file)) if preferred
site_data <- st_read(shp_file)
message(paste("Successfully loaded shapefile:", shp_file))

# ==========================
# 2️⃣ Extract First Letter of EENH1
# ==========================

# Create a new column with the first letter of EENH1 (in lowercase)
site_data <- site_data %>%
  mutate(EENH1_Group = str_to_lower(str_sub(EENH1, 1, 1)))  # Ensure lowercase

# ==========================
# 2.5 Filter and Summarize for Bar Plots (Grassland & Wetland Areas)
# ==========================

# Separate grassland and wetland data based on EENH1 first letter ('h' or 'm')
grassland_data_filtered <- site_data %>%
  st_drop_geometry() %>%
  filter(EENH1_Group == "h") # Grassland (EENH1 starts with 'h')

wetland_data_filtered <- site_data %>%
  st_drop_geometry() %>%
  filter(EENH1_Group == "m") # Wetland (EENH1 starts with 'm')

# Calculate total area per HAB1 for Grassland
grassland_area_summary <- grassland_data_filtered %>%
  group_by(HAB1) %>%
  summarise(Total_Area_ha = sum(OPPERVL, na.rm = TRUE) / 10000, .groups = "drop") # Convert m² to ha

# Calculate total area per HAB1 for Wetland
wetland_area_summary <- wetland_data_filtered %>%
  group_by(HAB1) %>%
  summarise(Total_Area_ha = sum(OPPERVL, na.rm = TRUE) / 10000, .groups = "drop") # Convert m² to ha


# ==========================
# 3️⃣ Cross-Tabulate EENH1 & HAB1 Based on Area
# ==========================

# Perform cross-tabulation, summing the polygon area (OPPERVL)
crosstab <- site_data %>%
  st_drop_geometry() %>%  # Remove geometry for tabular operations
  group_by(EENH1, HAB1) %>%
  summarise(Total_Area = sum(OPPERVL, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = HAB1, values_from = Total_Area, values_fill = 0)  
    # Convert to wide format

# Normalize the values to **relative proportions per HAB1**
crosstab_long <- site_data %>%
  st_drop_geometry() %>%
  group_by(EENH1, HAB1) %>%
  summarise(Total_Area = sum(OPPERVL, na.rm = TRUE), .groups = "drop") %>%
  group_by(HAB1) %>%
  mutate(Relative_Area = Total_Area / sum(Total_Area, na.rm = TRUE)) %>%
  ungroup()

# ==========================
# 4️⃣ Define the local output folder
# ==========================

# Output directory will *not* include the study site name directly
output_dir <- file.path("output", "BWK_exploration") 
# Create subfolders for better organization
output_heatmap_dir <- file.path(output_dir, "Crosstab_BWK")
output_gpkg_dir <- file.path(output_dir, "BWK_per_class")
output_bar_plots_dir <- file.path(output_dir, "Area_HAB1")

# Create all necessary output directories if they don't exist
if (!dir.exists(output_heatmap_dir)) {
  dir.create(output_heatmap_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(output_gpkg_dir)) {
  dir.create(output_gpkg_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(output_bar_plots_dir)) {
  dir.create(output_bar_plots_dir, recursive = TRUE, showWarnings = FALSE)
}


# ==========================
# 5️⃣Generate Heatmaps for EENH1 Starting with 'h' & 'm'
# ==========================

# Set target folder in Drive for heatmaps
target_folder_id_heatmaps <- "1AhkgIEDFaVP7KGcbBxA6lHT8hdAo5hiM"


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
    # Use the specific heatmap output directory and include site name in filename
    heatmap_file_path <- file.path(output_heatmap_dir, paste0(study_site_name, "_heatmap_", letter, ".png"))
    ggsave(filename = heatmap_file_path,
           plot = p, width = plot_width, height = plot_height, dpi = 300)
    
    # Upload file
    message(paste("Uploading heatmap:", basename(heatmap_file_path)))
    tryCatch({
      drive_upload(
        media = heatmap_file_path,
        path = as_id(target_folder_id_heatmaps),
        overwrite =  TRUE
      )
    }, error = function(e) {
      warning(paste("Failed to upload heatmap", basename(heatmap_file_path), ":", e$message))
    })
  } else {
    message(paste("No data to plot heatmap for EENH1 starting with", letter, " for site ", study_site_name))
  }
}

# ==========================================
# 5.5 Generate Ordered Bar Plots for HAB1 area (Grassland & Wetland)
# ==========================================

# Function to create an ordered bar plot
plot_bar_chart <- function(data, category_name, study_site) {
  ggplot(data, aes(x = reorder(HAB1, Total_Area_ha), y = Total_Area_ha, fill = Total_Area_ha)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = paste0(round(Total_Area_ha, 1), " ha")), hjust = -0.1, size = 4) + # Add hectare labels
    scale_fill_viridis_c() +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),    # removes all grid lines
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black")
    ) +
    labs(title = paste0("Total Area per HAB1 (", category_name, ") in ", study_site),
         x = "HAB1 Class", y = "Total Area (ha)") +
    coord_flip() # Flip axes for better readability
}

# Plot for Grassland
grassland_barplot <- plot_bar_chart(grassland_area_summary, "Grassland", study_site_name)

# Plot for Wetland
wetland_barplot <- plot_bar_chart(wetland_area_summary, "Wetland", study_site_name)

# Display the plots (optional, mainly for interactive use)
print(grassland_barplot)
print(wetland_barplot)

# Set target folder in Drive for bar plots
target_folder_id_barplots <- "1CpH2prLEeY4Jo0stJKmSdQutZrBPFsZJ" # Your target folder ID

# Save and Upload the Bar Plots
# Local file paths for bar plots
grassland_barplot_path <- file.path(output_bar_plots_dir, paste0(study_site_name, "_HAB1_Area_h.png"))
wetland_barplot_path <- file.path(output_bar_plots_dir, paste0(study_site_name, "_HAB1_Area_m.png"))


# Save the plots locally
ggsave(grassland_barplot_path, grassland_barplot, width = 10, height = 6, dpi = 300)
ggsave(wetland_barplot_path, wetland_barplot, width = 10, height = 6, dpi = 300)


# Upload plots to Google Drive
message(paste("Uploading grassland bar plot:", basename(grassland_barplot_path)))
tryCatch({
  drive_upload(
    media = grassland_barplot_path,
    path = as_id(target_folder_id_barplots),
    overwrite = TRUE
  )
}, error = function(e) {
  warning(paste("Failed to upload grassland bar plot:", basename(grassland_barplot_path), ":", e$message))
})

message(paste("Uploading wetland bar plot:", basename(wetland_barplot_path)))
tryCatch({
  drive_upload(
    media = wetland_barplot_path,
    path = as_id(target_folder_id_barplots),
    overwrite = TRUE
  )
}, error = function(e) {
  warning(paste("Failed to upload wetland bar plot:", basename(wetland_barplot_path), ":", e$message))
})

# ==========================
# 6️⃣ Save Separate Shapefiles for 'h' & 'm'
# ==========================
# Set target folder in Drive for GeoPackages (these are general output folders, not site-specific)
target_folder_id_gpkgs <- "1CNbyv0XRaMYUYMKfjfkpJ8nZYa-Xsd_F"

# Function to save filtered data as .gpkg
save_filtered_geopackage <- function(letter) {
  filtered_data <- filter(site_data, EENH1_Group == letter)
  
  if (nrow(filtered_data) > 0) {
    # Include site name in filename
    gpkg_path <- file.path(output_gpkg_dir, paste0(study_site_name, "_BWK2023_", letter, ".gpkg"))
    
    # Save as GeoPackage
    message(paste("Saving GeoPackage:", basename(gpkg_path)))
    tryCatch({
      st_write(filtered_data, gpkg_path, delete_dsn = TRUE)
    }, error = function(e) {
      stop(paste("Failed to save GeoPackage", basename(gpkg_path), ":", e$message))
    })
    
    # Upload to Google Drive
    message(paste("Uploading GeoPackage:", basename(gpkg_path)))
    tryCatch({
      googledrive::drive_upload(
        media = gpkg_path,
        path = as_id(target_folder_id_gpkgs),
        overwrite = TRUE
      )
      message(paste("Saved and uploaded:", basename(gpkg_path)))
    }, error = function(e) {
      warning(paste("Failed to upload GeoPackage", basename(gpkg_path), ":", e$message))
    })
    
  } else {
    message(paste("No features found for EENH1_Group =", letter, " for site ", study_site_name))
  }
}

# Save and upload for 'h' and 'm'
save_filtered_geopackage("h")
save_filtered_geopackage("m")
