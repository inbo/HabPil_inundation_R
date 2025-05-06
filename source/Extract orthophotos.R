# ==========================================
# 1️⃣ Load the Geopackages of the inundated habitat patches
# This section connects to Google Drive, lists available .gpkg files,
# and downloads them into a local folder if they haven't been downloaded already.
# ==========================================

library(googledrive)

# Google Drive folder name or ID (preferably use ID to avoid duplicates/mismatches)
drive_folder_id <- as_id("1PVztG2YqIYrErBATtiZ-6RELYm3pRjb3") 

# Set relative path to your local data folder
local_dir <- "data/Habitat patches inundated"

# Create local folder if needed
if (!dir.exists(local_dir)) {
  dir.create(local_dir, recursive = TRUE)
}

# List all files in the Google Drive folder
drive_files <- drive_ls(path = drive_folder_id)

# Optionally filter to .gpkg files only
gpkg_files <- drive_files[grepl("\\.gpkg$", drive_files$name), ]

# Download if not already present
for (i in seq_len(nrow(gpkg_files))) {
  file_name <- gpkg_files$name[i]
  local_path <- file.path(local_dir, file_name)
  
  if (!file.exists(local_path)) {
    cat("Downloading:", file_name, "\n")
    drive_download(file = gpkg_files[i, ], path = local_path, overwrite = FALSE)
  } else {
    cat("Already exists, skipping:", file_name, "\n")
  }
}

# ==========================================
# 2️⃣ Determine and prepare orthophoto tiles needed (download/copy first)
# This section reads each .gpkg file, extracts its bounding box,
# and determines which orthophoto tiles (based on kaartbladversnijding) are needed.
# Then it builds a unique set of tile codes to prepare.
# ==========================================

library(sf)
library(terra)

get_required_tiles <- function(kaartblad_path) {
  kaartblad <- st_read(kaartblad_path, quiet = TRUE)
  gpkg_dir <- "data/Habitat patches inundated"
  gpkg_files <- list.files(gpkg_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  tile_set <- character()
  
  for (gpkg in gpkg_files) {
    patch <- st_read(gpkg, quiet = TRUE)
    if (is.na(st_crs(patch))) {
      warning("No CRS found for: ", patch_name, " — skipping.")
      next
    }
    bbox <- st_as_sfc(st_bbox(patch))
    st_crs(bbox) <- st_crs(patch)
    bbox <- st_transform(bbox, st_crs(kaartblad))
    
    intersecting <- st_filter(kaartblad, bbox, .predicate = st_intersects)
    tile_set <- c(tile_set, intersecting$CODE)
  }
  
  tile_codes <- unique(tile_set)
  
  message("Found ", length(tile_codes), " unique tile codes needed.")
  print(tile_codes)
  
  return(tile_codes)
}

# ==========================================
# 3️⃣ Prepare orthophoto tiles (copy from source)
# This function copies required orthophoto .jp2 tiles from the central Z: drive
# to a local folder for faster and more reliable access during processing.
# ==========================================

prepare_tiles <- function(tile_codes, ortho_root, dest_dir = "data/orthophoto/tiles") {
  total <- length(tile_codes)
  message("Preparing ", total, " tile(s)...")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (i in seq_along(tile_codes)) {
    code <- tile_codes[i]
    message("[", i, "/", total, "] Processing tile code: ", code)
    parts <- unlist(strsplit(code, "/"))
    kaartblad <- as.integer(parts[1])
    subtile <- parts[2]
    
    folder <- if (kaartblad < 10) paste0("K0", kaartblad) else paste0("K", kaartblad)
    filename <- paste0("OMWRGB23VL_K", if (kaartblad < 10) paste0("0", kaartblad) else kaartblad, subtile, ".jp2")
    src_path <- file.path(ortho_root, folder, filename)
    dest_path <- file.path(dest_dir, filename)
    
    if (!file.exists(src_path)) {
      warning("Source tile not found: ", src_path)
    } else if (!file.exists(dest_path)) {
      file.copy(src_path, dest_path)
      message("✅ Copied: ", filename)
    } else {
      message("⏩ Already exists: ", filename)
    }
  }
}

# ==========================================
# 5️⃣ Clip orthophotos using bounding box of each habitat patch
# This function loops over each .gpkg file, finds the intersecting tiles,
# loads them, and saves cropped GeoTIFFs (one per tile) into a 2023 subfolder.
# ==========================================

clip_orthos_from_bbox <- function(
    kaartblad_path,
    ortho_root,
    output_dir = "output/clipped_orthos"
) {
  # Create output dir if needed
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Read kaartbladversnijding
  kaartblad <- st_read(kaartblad_path, quiet = TRUE)
  
  # List all GPKGs in the hardcoded input folder
  gpkg_dir <- "data/Habitat patches inundated"
  gpkg_files <- list.files(gpkg_dir, pattern = "\\.gpkg$", full.names = TRUE)
  
  for (gpkg in gpkg_files) {
    patch_name <- tools::file_path_sans_ext(basename(gpkg))
    year_subfolder <- file.path(output_dir, "2023")
    expected_prefix <- paste0(patch_name, "_")
    existing_outputs <- list.files(year_subfolder, pattern = paste0("^", expected_prefix), full.names = TRUE)
    
    if (length(existing_outputs) > 0) {
      message("⏩ Already processed: ", patch_name)
      next
    }
    message("\nProcessing: ", patch_name)
    
    patch <- st_read(gpkg, quiet = TRUE)
    if (is.na(st_crs(patch)) || is.na(st_crs(patch)$epsg) || (isFALSE(sf::st_is_longlat(patch)) && st_crs(patch)$epsg < 100)) {
      warning("Invalid or missing CRS for: ", patch_name, " — skipping.")
      next
    }
    bbox <- st_as_sfc(st_bbox(patch))
    st_crs(bbox) <- st_crs(patch)
    bbox <- st_transform(bbox, st_crs(kaartblad))
    
    # Intersect bbox with kaartblad polygons
    intersecting <- st_filter(kaartblad, bbox, .predicate = st_intersects)
    if (nrow(intersecting) == 0) {
      warning("No matching orthophoto tiles for: ", patch_name)
      next
    }
    
    rasters <- list()
    
    message("Intersecting kaartblad tiles:")
    print(intersecting$CODE)
    
    for (i in seq_len(nrow(intersecting))) {
      code <- intersecting$CODE[i]  # format: "2/5z"
      parts <- unlist(strsplit(code, "/"))
      kaartblad <- as.integer(parts[1])
      subtile <- parts[2]  # e.g. "5z"
      
      folder <- if (kaartblad < 10) paste0("K0", kaartblad) else paste0("K", kaartblad)
      filename <- paste0("OMWRGB23VL_K", if (kaartblad < 10) paste0("0", kaartblad) else kaartblad, subtile, ".jp2")
      filepath <- file.path("data/orthophoto/tiles", filename)
      
      message("  → Checking tile: ", filename)
      
      if (!file.exists(filepath)) {
        warning("Missing tile: ", filepath)
        next
      }
      
      message("     ✅ Tile found and loaded: ", filepath)
      rasters[[length(rasters) + 1]] <- rast(filepath)
    }
    
    if (length(rasters) == 0) {
      warning("No rasters found for patch: ", patch_name)
      next
    }
    
    for (j in seq_along(rasters)) {
      code <- intersecting$CODE[j]
      parts <- unlist(strsplit(code, "/"))
      kaartblad <- as.integer(parts[1])
      subtile <- parts[2]
      year_subfolder <- file.path(output_dir, "2023")
      dir.create(year_subfolder, recursive = TRUE, showWarnings = FALSE)
      
      filename <- paste0(patch_name, "_", kaartblad, subtile, ".tif")
      tile_out_path <- file.path(year_subfolder, filename)
      
      tile_cropped <- crop(rasters[[j]], vect(bbox))
      writeRaster(tile_cropped, tile_out_path, overwrite = TRUE,
                  gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
      message("✅ Saved individual tile: ", tile_out_path)
    }
    
  }
  
  message("
🎉 All done.")
}


# ==========================================
# 6️⃣ Execute the workflow: get tiles, copy files, and clip orthophotos
# This section runs all steps in order: determine needed tiles, copy them, then clip per patch.
# ==========================================

# Set paths
kaartblad_path <- "data/Orthophotos/Kaartbladversnijding/Kbl16.shp"
ortho_root <- "Z:/Vlaanderen/Referentie/Orthofoto/Orthofoto_KL_2023_vl"

# Run tile detection and copying
tile_codes <- get_required_tiles(kaartblad_path)
prepare_tiles(tile_codes, ortho_root)

# Run clipping
clip_orthos_from_bbox(
  kaartblad_path = kaartblad_path,
  ortho_root = ortho_root
)
