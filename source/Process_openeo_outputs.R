library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr) 


# Load the data
mcfeeters_path <- "output/NDWI_McFeeters/Demervallei/masked/timeseries.json"
xu_path <- "output/NDWI_Xu/Demervallei/masked/timeseries.json"
gao_path <- "output/NDWI_Gao/Demervallei/masked/timeseries.json"


ndwi_mcfeeters <- fromJSON(mcfeeters_path)
ndwi_xu <- fromJSON(xu_path)
ndwi_gao <- fromJSON(gao_path)

ndwi_mcfeeters_df <- do.call(rbind, lapply(names(ndwi_mcfeeters), function(date) {
  data.frame(
    date = as.Date(date),
    point_id = seq_along(ndwi_mcfeeters[[date]]),  # Assign unique point IDs
    NDWI_McFeeters = unlist(ndwi_mcfeeters[[date]])  # Extract values
  )
}))

ndwi_xu_df <- do.call(rbind, lapply(names(ndwi_xu), function(date) {
  data.frame(
    date = as.Date(date),
    point_id = seq_along(ndwi_xu[[date]]),
    NDWI_Xu = unlist(ndwi_xu[[date]])
  )
}))

ndwi_gao_df <- do.call(rbind, lapply(names(ndwi_gao), function(date) {
  data.frame(
    date = as.Date(date),
    point_id = seq_along(ndwi_gao[[date]]),
    NDWI_Gao = unlist(ndwi_gao[[date]])
  )
}))


ndwi_df <- reduce(list(ndwi_mcfeeters_df, ndwi_xu_df, ndwi_gao_df), full_join, by = c("date", "point_id"))

# Ensure date column is in Date format
ndwi_df$date <- as.Date(ndwi_df$date)

# Ensure the output directory exists
output_dir <- "output/NDWI_time_series/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create directory if it doesn't exist
}

# Save the CSV file
write.csv(ndwi_df, file.path(output_dir, "Demervallei_NDWI_all.csv"), row.names = FALSE)



