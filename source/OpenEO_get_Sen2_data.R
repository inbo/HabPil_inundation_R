# Load required libraries
library(sf)
library(openeo)
library(ggplot2)
library(readr)
library(terra)

# -----------------------------------------------
# 1. Load and Transform Shapefile of Subsite
# -----------------------------------------------
# Load polygons (in Belge Lambert72)
plot_poly <- st_read("data/Habitat patches inundated/Web_broek_extra.geojson")

# Convert to WGS 84 (EPSG:4326) for OpenEO processing
plot_poly_wgs84 <- st_transform(plot_poly, crs = 4326)

# -----------------------------------------------
# 2 Connect to OpenEO Backend
# -----------------------------------------------
con <- connect("https://openeo.dataspace.copernicus.eu")
login()

# Define bounding box from transformed points
aoi <- list(
  west = min(st_bbox(plot_poly)["xmin"]),
  east = max(st_bbox(plot_poly)["xmax"]),
  south = min(st_bbox(plot_poly)["ymin"]),
  north = max(st_bbox(plot_poly)["ymax"])
)

# -----------------------------------------------
# 4 Load Sentinel-2 Data 
# -----------------------------------------------
processes <- processes()

datacube <- processes$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = aoi,
  temporal_extent = c("2023-03-01", "2023-03-01")
)

# -----------------------------------------------
# 5 Create and Start OpenEO Job
# -----------------------------------------------
job = create_job(graph=datacube, title = "Web_broek_extra_2023_Rjob", description = "Getting Sen2 data for Web_broek_extra 2023")
start_job(job=job)

list_jobs()

# -----------------------------------------------
# 6 Download OpenEO data
# -----------------------------------------------
download_results(job=job, folder = "data/Sen2/Web_broek_extra/2023")
