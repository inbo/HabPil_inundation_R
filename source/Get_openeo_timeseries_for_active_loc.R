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
# 2️⃣ List available collections and processes
# -----------------------------------------------
p = processes() # to use
coll_vito <- list_collections() # to check
proc_vito <- list_processes() # to check
formats = list_file_formats()

# -----------------------------------------------
# 2️⃣ Get data from collection
# -----------------------------------------------

data <- p$load_collection(
  id = "TERRASCOPE_S2_TOC_V2",
  spatial_extent = aoi,
  temporal_extent = c("2024-06-01", "2024-06-30"),
  bands = c("B04", "B08"), 
)

spectral_reduce = p$reduce_dimension(data = data, dimension = "bands",reducer = function(data,context) {
  b8 = data[1]
  b4 = data[2]
  
  return((b8-b4)/(b8+b4))
})

temporal_reduce = p$reduce_dimension(data=spectral_reduce,dimension = "t", reducer = function(x,y){
  max(x)
})

apply_linear_transform = p$apply(data=temporal_reduce,process = function(value,...) {
  p$linear_scale_range(x = value, 
                       inputMin = -1, 
                       inputMax = 1, 
                       outputMin = 0, 
                       outputMax = 255)
})

result = p$save_result(data=apply_linear_transform, format=formats$output$netCDF, options = list(filename_prefix="TEST"))

job_maxNDVI = create_job(graph=result,title = "Maximum NDVI", description = "Maximum NDVI calculation on Sentinel-2 data, and exporting as PNG file.")
describe_job(job = job_maxNDVI)

start_job(job=job_maxNDVI)
list_jobs()
list_results(job=job_maxNDVI)
download_results(job=job_maxNDVI, folder = "output/")
