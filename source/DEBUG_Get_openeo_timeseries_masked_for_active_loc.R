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
  temporal_extent = c("2024-01-01", "2024-12-31"),
  bands = c("B03", "B08", "B11", "SCL"), 
)

# Compute and rename NDWI indices
ndwi_mcfeeters <- p$rename_labels(
  p$reduce_dimension(
    data = data,
    dimension = "bands",
    reducer = function(data, context) {
      (data[1] - data[2]) / (data[1] + data[2])  # NDWI McFeeters
    }
  ),
  dimension = "bands",
  target = list("NDWI_McFeeters")
)

ndwi_xu <- p$rename_labels(
  p$reduce_dimension(
    data = data,
    dimension = "bands",
    reducer = function(data, context) {
      (data[1] - data[3]) / (data[1] + data[3])  # NDWI Xu
    }
  ),
  dimension = "bands",
  target = list("NDWI_Xu")
)

ndwi_gao <- p$rename_labels(
  p$reduce_dimension(
    data = data,
    dimension = "bands",
    reducer = function(data, context) {
      (data[2] - data[3]) / (data[2] + data[3])  # NDWI Gao
    }
  ),
  dimension = "bands",
  target = list("NDWI_Gao")
)

# Merge indices step-by-step
indices_data <- p$merge_cubes(
  p$merge_cubes(ndwi_mcfeeters, ndwi_xu),  
  ndwi_gao  
)



masked_indices <- p$mask(
  data = indices_data,  # The computed NDWI indices
  mask = p$apply(
    data = data,  # Use the original data to extract SCL
    process = function(data, context) {
      scl <- data[4]  # SCL band (4th in list)
      invalid_mask <- p$or(scl < 4, scl > 6)  # Mask out clouds & invalid pixels
      return(invalid_mask)  # Boolean mask (TRUE for invalid, FALSE for valid)
    }
  )
)

# Convert each point into a valid GeoJSON Feature
plot_points_geojson <- list(
  type = "FeatureCollection",
  features = lapply(1:nrow(plot_points_subset), function(i) {
    list(
      type = "Feature",
      geometry = fromJSON(sf_geojson(st_sf(geometry = st_geometry(plot_points_subset[i, , drop = FALSE])))),  
      properties = list(id = i)  # Optional: Add an ID for tracking
    )
  })
)

# aggregate spatial to only keep values at point locations
ndwi_plots <- p$aggregate_spatial(
  data = masked_indices ,
  geometries = plot_points_geojson,
  reducer = p$mean  # Compute mean NDWI in each buffer
)

# create a process graph to save the result
result = p$save_result(data=ndwi_plots, format=formats$output$JSON, options = list(filename_prefix="NDWI_Demer_points"))

# Make a job to run on openeo backend
job_NDWI = create_job(graph=result,title = "NDWI_all", description = "Exporting the time series of NDWI for Demervallei")
describe_job(job = job_NDWI)

# Start the job
start_job(job=job_NDWI)

# Check the status of the job
list_jobs()
list_results(job=job_NDWI)

# Download the results
download_results(job=job_NDWI, folder = "output/NDWI_all/Demervallei/masked")

--------------------------------------------------------------------------------

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
  features = lapply(1:nrow(plot_points_subset), function(i) {
    list(
      type = "Feature",
      geometry = fromJSON(sf_geojson(st_sf(geometry = st_geometry(plot_points_subset[i, , drop = FALSE])))),  
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
--------------------------------------------------------------------------------


scl <- p$reduce_dimension(
    data = data,
    dimension = "bands",
    reducer = function(data, context) {
      scl <- data[4] 
      return(scl)
    }
  )

# create a process graph to save the SCL
result_scl = p$save_result(data=scl, format=formats$output$GTiff, options = list(filename_prefix="NDWI_Peeters_Demer_masked"))


# Make a job to run on openeo backend
job_scl = create_job(graph=result_scl,title = "SCL", description = "Exporting the SCL for Demervallei")
describe_job(job = job_scl)

# Start the job
start_job(job=job_scl)

# Check the status of the job
list_jobs()
list_results(job=job_scl)

# Download the results
download_results(job=job_scl, folder = "output/NDWI_Peeters/Demervallei/SCL")


