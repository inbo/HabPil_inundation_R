# Load required libraries
library(sf)
library(openeo)
library(ggplot2)
library(readr)
library(terra)

# -----------------------------------------------
# 1. Load and Transform Point Locations
# -----------------------------------------------
# Load points (in Belge Lambert72)
plot_points <- st_read("output/meetpunten_Demervallei_actief2018.geojson")

# Convert to WGS 84 (EPSG:4326) for OpenEO processing
plot_points_wgs84 <- st_transform(plot_points, crs = 4326)

# Select only the first 5 points
plot_points_subset <- plot_points_wgs84[1:5, ]

# -----------------------------------------------
# 2️⃣ Connect to OpenEO Backend
# -----------------------------------------------
con <- connect("https://openeo.vito.be")
login()

# Define bounding box from transformed points
aoi <- list(
  west = min(st_bbox(plot_points_subset)["xmin"]),
  east = max(st_bbox(plot_points_subset)["xmax"]),
  south = min(st_bbox(plot_points_subset)["ymin"]),
  north = max(st_bbox(plot_points_subset)["ymax"])
)

# -----------------------------------------------
# 3️⃣ Load Sentinel-2 Data (2024) with SCL Band
# -----------------------------------------------
processes <- processes()

datacube <- processes$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = aoi,
  temporal_extent = c("2024-01-01", "2024-12-31"),
  bands = c("B08", "B11", "SCL"),  # NIR, SWIR, and SCL for masking
)

# -----------------------------------------------
# 4️⃣ Apply Cloud & Water Mask (Keep SCL = 4, 5, 6)
# -----------------------------------------------
scl <- processes$filter_bands(data = datacube, bands = "SCL")

# Keep only pixels where SCL = 4 (Vegetation), 5 (Not Vegetated), or 6 (Water)
valid_scl_mask <- processes$or(
  processes$or(
    processes$eq(scl, 4),  # Vegetation
    processes$eq(scl, 5)   # Not Vegetated
  ),
  processes$eq(scl, 6)     # Water
)

# Apply the mask to the data
masked_data <- processes$mask(
  data = datacube,
  mask = valid_scl_mask
)

# -----------------------------------------------
# 5️⃣ Compute NDMI (Using Masked Data)
# -----------------------------------------------
# NDMI = (NIR - SWIR) / (NIR + SWIR)
ndmi_cube <- processes$reduce_dimension(
  data = masked_data,
  dimension = "bands",
  reducer = processes$normalized_difference(x = "B08", y = "B11")
)

# -----------------------------------------------
# 6️⃣ Extract NDMI and Band 11 Time Series at 5 Points
# -----------------------------------------------
ndmi_ts <- processes$aggregate_spatial(
  data = ndmi_cube,
  geometries = plot_points_subset,
  reducer = processes$mean
)

b11_ts <- processes$aggregate_spatial(
  data = masked_data,
  geometries = plot_points_subset,
  reducer = processes$mean()
)

# -----------------------------------------------
# 7️⃣ Create and Start OpenEO Job
# -----------------------------------------------
job <- ndmi_ts$create_job(
  title = "NDMI_Extraction_5_Points",
  description = "Extracting NDMI for 5 points"
)

# Start the job
job$start()

# -----------------------------------------------
# 8️⃣ Check Job Status Until It’s Finished
# -----------------------------------------------
repeat {
  status <- job$status()
  print(paste("Job Status:", status))
  
  if (status == "finished") {
    break
  } else if (status == "error") {
    stop("Job failed!")
  } else {
    Sys.sleep(30)  # Wait 30 seconds before checking again
  }
}

# -----------------------------------------------
# 9️⃣ Download Results Once Job is Finished
# -----------------------------------------------
output_dir <- "output/ndmi_results/"
job_results <- job$download_results(target = output_dir)

# -----------------------------------------------
# 🔟 Load the Downloaded Results into R
# -----------------------------------------------
# Load NDMI raster if it was saved as GeoTIFF
ndmi_raster_path <- file.path(output_dir, "ndmi.tif")  # Adjust filename if needed
if (file.exists(ndmi_raster_path)) {
  ndmi_raster <- rast(ndmi_raster_path)
  plot(ndmi_raster)  # Quick visualization
}

# Load NDMI data if it was saved as a CSV
ndmi_csv_path <- file.path(output_dir, "ndmi.csv")  # Adjust filename if needed
if (file.exists(ndmi_csv_path)) {
  ndmi_df <- read.csv(ndmi_csv_path)
  head(ndmi_df)  # Preview
}

# -----------------------------------------------
# 🎉 Done! NDMI Time Series Successfully Extracted for 5 Points!
# ------------------------------------