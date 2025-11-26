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
#ADD FOR SENTINEL-2 TILE


# Root for data and output 
data_root_dir <- "data"       
output_root_dir <- "output"   


# Specific subfolder names for datasets 
labels_subfolder_name <- "Final labeled/output_step1_pythonLabelMe"
tiles_subfolder_name <- "Tiles"
#sen2_subfolder_name <- "Sen2"
labels_filename <- "polygons_py.shp"
tiles_filename <- "Tiles_sel.shp"
#sen2_filename <- "Sen2_Web_broek_extra_2023.tif" #ADAPT TO CONSTRUCT FROM study_site_name & study_year


# Other project-specific parameters
tile_id_column_name  <- "TileID"
background_label <- "not inundated"

message("Configuration loaded from config.R")