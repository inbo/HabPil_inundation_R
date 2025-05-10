# Configuration file

# Study site and year 
study_site_name <- "Web_broek_extra"   #ADAPT
# Possible values: Web_broek_extra, Grootbroek_gras, Grootbroek_wet, 
# Schulensmeer_gras, Schulensmeer_wet, Kloosterbeemden_gras,
# Kloosterbeemden_wet, Oudheverlee_gras, Oudheverlee_wet, Webbekomsbroek_gras, 
# Webbekomsbroek_wet

study_year <- "2023"                    #ADAPT
# Possible values: 2019, 2020, 2021, 2022, 2023, 2024

# Google Drive IDs of folders
gdrive_id_labels <- "1C9D0S_pLUjYqRBaOaJUliJk_PbJd_qpP" # ADAPT
gdrive_id_tiles <- "1c8MGM_ZPO4cCfVyNz8lM6YeD07LTFGjC" # ADAPT


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
labels_subfolder_name <- "Final labeled/output_step1_pythonLabelMe"
tiles_subfolder_name <- "Tiles"
labels_filename <- "polygons_py.shp"
tiles_filename <- "Tiles_sel.shp"


# Other project-specific parameters
tile_id_column_name  <- "TileID"
background_label <- "not inundated"
output_gpkg_filename <- paste0(study_site_name, "_", study_year, 
                               "_final_labeled.gpkg") # Dynamic filename

message("Configuration loaded from config.R")