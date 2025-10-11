# ==============================================================================
# Calculate and Compare Overall Performance Across All Study Sites
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all 'final_pixel_attributes.csv' files for both
# original and super-resolution results. It then calculates a single,
# comprehensive set of performance metrics for each resolution by treating all
# combined pixels as a single dataset.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(stringr)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
# The root directory where the individual pixel attribute files are stored
pixel_data_root_dir <- "output/pixel_data_tables"

# The directory where the final summary CSV will be saved
summary_output_dir <- "output/summary_plots"
dir.create(summary_output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1️⃣ Define Performance Calculation Function
# ==============================================================================

#' Calculate performance metrics on an aggregated data frame
#'
#' @param df A data frame containing 'dominant_label' and 'predicted_jussila' columns.
#' @param resolution_name A character string (e.g., "Original") to label the output.
#' @return A single-row data frame summarizing all performance metrics.
calculate_overall_performance <- function(df, resolution_name) {
  
  message(paste(" -> Calculating metrics for:", resolution_name, "(", nrow(df), "total pixels)"))
  
  # Map dominant labels to 'water', 'dry', or NA for evaluation
  evaluation_df <- df %>%
    mutate(reference_class = case_when(
      dominant_label == 'Inundated'     ~ 'water',
      dominant_label == 'Not inundated' ~ 'dry',
      TRUE                              ~ NA_character_
    )) %>%
    filter(!is.na(reference_class))
  
  message(paste("    Evaluating on", nrow(evaluation_df), "classified pixels (Inundated or Not inundated)"))
  
  # Calculate confusion matrix components for both classes
  tp_water <- sum(evaluation_df$reference_class == "water" & evaluation_df$predicted_jussila == "water")
  fp_water <- sum(evaluation_df$reference_class == "dry"   & evaluation_df$predicted_jussila == "water")
  fn_water <- sum(evaluation_df$reference_class == "water" & evaluation_df$predicted_jussila == "dry")
  
  tp_dry <- sum(evaluation_df$reference_class == "dry"   & evaluation_df$predicted_jussila == "dry")
  fp_dry <- sum(evaluation_df$reference_class == "water" & evaluation_df$predicted_jussila == "dry")
  fn_dry <- sum(evaluation_df$reference_class == "dry"   & evaluation_df$predicted_jussila == "water")
  
  # --- Calculate Metrics (with safeguards for division by zero) ---
  recall_water    <- tp_water / (tp_water + fn_water)
  precision_water <- tp_water / (tp_water + fp_water)
  f1_water        <- 2 * (precision_water * recall_water) / (precision_water + recall_water)
  
  recall_dry      <- tp_dry / (tp_dry + fn_dry)
  precision_dry   <- tp_dry / (tp_dry + fp_dry)
  f1_dry          <- 2 * (precision_dry * recall_dry) / (precision_dry + recall_dry)
  
  macro_f1        <- (f1_water + f1_dry) / 2
  
  # Create the summary data frame
  summary_df <- tibble(
    Resolution = resolution_name,
    `Macro F1` = macro_f1,
    `F1 Water` = f1_water,
    `F1 Dry` = f1_dry,
    `Recall Water` = recall_water,
    `Precision Water` = precision_water,
    `Recall Dry` = recall_dry,
    `Precision Dry` = precision_dry,
    `Total Pixels Evaluated` = nrow(evaluation_df)
  ) %>%
    # Replace any potential NaN values with 0
    mutate(across(where(is.numeric), ~if_else(is.nan(.), 0, .)))
  
  return(summary_df)
}

# ==============================================================================
# 2️⃣ Aggregate All Pixel Data
# ==============================================================================
message("--- Searching for and aggregating all pixel attribute files ---")

# Find all relevant CSV files
all_files <- list.files(
  path = pixel_data_root_dir,
  pattern = "_final_pixel_attributes.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Separate files by resolution type
original_files <- all_files[!grepl("_superres", all_files)]
superres_files <- all_files[grepl("_superres", all_files)]

message(paste("Found", length(original_files), "'original' resolution files."))
message(paste("Found", length(superres_files), "'superres' resolution files."))

# --- Load and combine ORIGINAL files ---
if (length(original_files) > 0) {
  all_original_pixels_df <- lapply(original_files, read.csv) %>%
    bind_rows() %>%
    # Standardize column names to lowercase for consistency
    rename_with(tolower)
} else {
  all_original_pixels_df <- NULL
}

# --- Load and combine SUPERRES files ---
if (length(superres_files) > 0) {
  all_superres_pixels_df <- lapply(superres_files, read.csv) %>%
    bind_rows() %>%
    # Standardize column names to lowercase for consistency
    rename_with(tolower)
} else {
  all_superres_pixels_df <- NULL
}

# ==============================================================================
# 3️⃣ Calculate and Display Final Comparison Table
# ==============================================================================
message("\n--- Calculating overall performance across all combined data ---")

# Create an empty list to hold the results
results_list <- list()

# Calculate performance for the original data, if it exists
if (!is.null(all_original_pixels_df)) {
  results_list$original <- calculate_overall_performance(all_original_pixels_df, "Original")
}

# Calculate performance for the super-resolution data, if it exists
if (!is.null(all_superres_pixels_df)) {
  results_list$superres <- calculate_overall_performance(all_superres_pixels_df, "Super-Resolution")
}

# --- Combine results into a final table ---
if (length(results_list) > 0) {
  final_summary_table <- bind_rows(results_list)
  
  message("\n--- Overall Performance Comparison Across All Study Sites ---")
  print(final_summary_table, width = Inf)
  
  # --- Save the final summary to a CSV file ---
  output_filename <- file.path(summary_output_dir, "overall_performance_summary.csv")
  write.csv(final_summary_table, output_filename, row.names = FALSE)
  message(paste("\nSUCCESS: Overall summary table saved to:\n", output_filename))
  
} else {
  message("\nWARNING: No pixel data was found to calculate overall performance.")
}

