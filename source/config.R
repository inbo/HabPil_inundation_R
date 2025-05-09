# Configuration file

# Study site and year 
study_site_name <- "Web_broek_extra"   #ADAPT
study_year <- "2023"                    #ADAPT


# Root for data and output 
data_root_dir <- "data"       #ADAPT
output_root_dir <- "output"   #ADAPT

my_base_dir <- file.path(data_root_dir, "LabelMe", study_site_name, study_year)
if (!dir.exists(my_base_dir )) {
  message("Local cache directory '", my_base_dir , "' does not exist. Creating it.")
  dir.create(my_base_dir , recursive = TRUE, showWarnings = TRUE)
} else {
  message("Using existing local cache directory: ", my_base_dir )
}

# Specific subfolder names for datasets 
polygons_subfolder_name <- "Final labeled/output_step1_pythonLabelMe"
tiles_subfolder_name <- "Tiles"

# Base directory for local data downloads


# Other project-specific parameters
tile_id_col <- "TileID"
background_val <- "not inundated"
output_gpkg_filename <- paste0(study_site_name, "_", study_year, 
                               "_final_labeled.gpkg") # Dynamic filename

# Google Drive IDs and target filenames
gdrive_id_labels <- "1C9D0S_pLUjYqRBaOaJUliJk_PbJd_qpP" # Your actual ID
target_filename_labels <- "polygons_py.shp"

gdrive_id_tiles <- "1c8MGM_ZPO4cCfVyNz8lM6YeD07LTFGjC" # Your actual ID
target_filename_tiles <- "Tiles_sel.shp"

message("Configuration loaded from config.R")