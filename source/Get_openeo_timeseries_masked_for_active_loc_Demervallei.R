# Load required libraries
library(sf)
library(openeo)
library(ggplot2)
library(readr)
library(terra)
library(jsonlite)
library(geojsonsf)


# -----------------------------------------------
# 1. Load and Transform Point Locations
# -----------------------------------------------
# Load points (in Belge Lambert72)
plot_points_wgs84 <- st_read("output/meetpunten_Demervallei_actief2018_WGS84.geojson")

# -----------------------------------------------
# 2️⃣ Connect to OpenEO Backend
# -----------------------------------------------
con <- connect("https://openeo.vito.be")

login()


# Define bounding box from transformed points

aoi <- list(
  west = min(st_bbox(plot_points_wgs84)["xmin"]),
  east = max(st_bbox(plot_points_wgs84)["xmax"]),
  south = min(st_bbox(plot_points_wgs84)["ymin"]),
  north = max(st_bbox(plot_points_wgs84)["ymax"])
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
  temporal_extent = c("2018-01-01", "2024-12-31"),
  bands = c("B03", "B08", "B11", "SCL"), 
)

# Function to compute and save NDWI
compute_and_save_ndwi <- function(index_name, band1, band2, filename_prefix) {
  # Compute the NDWI index
  ndwi <- p$reduce_dimension(
    data = data,
    dimension = "bands",
    reducer = function(data, context) {
      (data[band1] - data[band2]) / (data[band1] + data[band2])
    }
  )
  
  # Mask using the SCL band
  masked_ndwi <- p$mask(
    data = ndwi,  
    mask = p$apply(
      data = data,  
      process = function(data, context) {
        scl <- data[4]  # SCL band (4th in list)
        invalid_mask <- p$or(scl < 4, scl > 6)  # Mask clouds & invalid pixels
        return(invalid_mask)  
      }
    )
  )
  
  # Aggregate at point locations
  ndwi_plots <- p$aggregate_spatial(
    data = masked_ndwi,
    geometries = plot_points_geojson,
    reducer = p$mean  
  )
  
  # Save the result
  result = p$save_result(data=ndwi_plots, format=formats$output$JSON, options = list(filename_prefix=filename_prefix))
  
  # Submit job to OpenEO backend
  job = create_job(graph=result, title = index_name, description = paste("Exporting", index_name, "for Demervallei"))
  start_job(job=job)
  
  return(job)
}

# Convert each point into a valid GeoJSON Feature
plot_points_geojson <- list(
  type = "FeatureCollection",
  features = lapply(1:nrow(plot_points_wgs84), function(i) {
    list(
      type = "Feature",
      geometry = fromJSON(sf_geojson(st_sf(geometry = st_geometry(plot_points_wgs84[i, , drop = FALSE])))),  
      properties = list(id = i)  
    )
  })
)


# Run jobs for each NDWI index separately
job_mcfeeters <- compute_and_save_ndwi("NDWI_McFeeters", 1, 2, "NDWI_McFeeters_Demer_points")
job_xu <- compute_and_save_ndwi("NDWI_Xu", 1, 3, "NDWI_Xu_Demer_points")
job_gao <- compute_and_save_ndwi("NDWI_Gao", 2, 3, "NDWI_Gao_Demer_points")


list_jobs()

download_results(job=job_mcfeeters, folder = "output/NDWI_McFeeters/Demervallei/masked")
download_results(job=job_xu, folder = "output/NDWI_Xu/Demervallei/masked")
download_results(job=job_gao, folder = "output/NDWI_Gao/Demervallei/masked")
