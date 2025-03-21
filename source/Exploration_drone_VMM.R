library(terra)
library(sf)
library(dplyr)
library(progressr)
library(purrr)

# Setup progress bar
handlers(global = TRUE)
handlers("txtprogressbar")

# Paths
img_dir <- "data/Luchtbeelden drones VMM"
output_gpkg <- "output/VMM drone imagery/drone_image_footprints.gpkg"

# List all .tif files recursively
img_files <- list.files(img_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# List to track skipped files
skipped <- list()

# Function to extract metadata + polygon footprint
get_img_info <- function(file) {
  tryCatch({
    # Safely read raster, suppressing GDAL noise
    r <- suppressWarnings(rast(file))
    
    # Confirm it's valid
    if (!inherits(r, "SpatRaster")) {
      skipped[[file]] <<- "Not a valid raster object"
      return(NULL)
    }
    
    crs_r <- crs(r)
    
    # Check if CRS can be interpreted by sf
    crs_info <- tryCatch(sf::st_crs(crs_r), error = function(e) NA)
    if (is.na(crs_info) || is.na(crs_info$epsg)) {
      skipped[[file]] <<- "Missing or non-convertible CRS (e.g. Engineering CRS)"
      return(NULL)
    }
    
    # Create polygon from extent
    poly <- tryCatch({
      ext_poly <- as.polygons(ext(r), crs = crs_r)
      sf::st_as_sf(ext_poly)
    }, error = function(e) {
      skipped[[file]] <<- paste("Failed to convert extent to polygon:", e$message)
      return(NULL)
    })
    
    if (is.null(poly)) return(NULL)
    
    # Reproject to EPSG:31370 (Lambert 72)
    poly <- tryCatch({
      st_transform(poly, 31370)
    }, error = function(e) {
      skipped[[file]] <<- paste("Failed to reproject to EPSG:31370:", e$message)
      return(NULL)
    })
    
    # Extract metadata
    fname <- basename(file)
    date_str <- substr(fname, 1, 6)
    flight_date <- tryCatch(as.Date(date_str, format = "%y%m%d"), error = function(e) NA)
    
    poly$file <- fname
    poly$path <- file
    poly$date <- flight_date
    poly$source_epsg <- crs_info$epsg
    
    return(poly)
  }, error = function(e) {
    skipped[[file]] <<- paste("Raster read error:", e$message)
    return(NULL)
  })
}

# Run processing with progress bar
footprints <- with_progress({
  p <- progressor(length(img_files))
  
  results <- lapply(img_files, function(file) {
    p(message = basename(file))
    get_img_info(file)
  })
  
  compact(results) |> bind_rows()
})

# Save outputs
if (nrow(footprints) > 0) {
  st_write(footprints, output_gpkg, delete_dsn = TRUE)
  cat("✅ GeoPackage saved in EPSG:31370 at:", output_gpkg, "\n")
} else {
  cat("⚠️ No valid images were processed.\n")
}

# Save skipped log
if (length(skipped) > 0) {
  skipped_df <- data.frame(
    file = names(skipped),
    reason = unlist(skipped),
    stringsAsFactors = FALSE
  )
  write.csv(skipped_df, "output/skipped_drone_files.csv", row.names = FALSE)
  cat("⚠️ Skipped files saved to: output/skipped_drone_files.csv\n")
  
  # Optional: print quick summary
  print(table(skipped_df$reason))
}
