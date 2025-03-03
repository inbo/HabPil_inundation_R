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
plot_points_wgs84 <- st_read("output/meetpunten_Demervallei_actief2018_WGS84.geojson")

plot_points_subset <- plot_points_wgs84[1:5,]
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
formats = list_file_formats()# to check

# -----------------------------------------------
# 2️⃣ Get data from collection
# -----------------------------------------------

data <- p$load_collection(
  id = "TERRASCOPE_S2_TOC_V2",
  spatial_extent = aoi,
  temporal_extent = c("2024-01-01", "2024-12-31"),
  bands = c("B03", "B08", "B11"), 
)

ndwi_peeters = p$reduce_dimension(data = data, dimension = "bands",reducer = function(data,context) {
  b3 = data[1]
  b8 = data[2]
  
  return((b3-b8)/(b3+b8))
})


library(jsonlite)

# Read the GeoJSON file into a nested list
plot_points_geojson <- fromJSON("output/meetpunten_Demervallei_actief2018_WGS84.geojson", simplifyVector = FALSE)

# Use it in filter_spatial()
ndwi_peeters_points <- p$filter_spatial(
  data = ndwi_peeters,
  geometries = plot_points_geojson
)

result = p$save_result(data=ndwi_peeters_points, format=formats$output$JSON, options = list(filename_prefix="NDWI_Peeters_Demer"))

job_NDWI_p = create_job(graph=result,title = "NDWI_Peeters", description = "Exporting the time series of NDWI_Peeters for Demervallei")
describe_job(job = job_NDWI_p)

start_job(job=job_NDWI_p)
list_jobs()
list_results(job=job_NDWI_p)
download_results(job=job_NDWI_p, folder = "output/")
