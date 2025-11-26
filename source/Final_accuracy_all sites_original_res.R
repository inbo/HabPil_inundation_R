# ==============================================================================
# Compare Overall Performance of Jussila vs. WiW Models
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all 'final_pixel_attributes.csv' files for the
# original resolution. It then calculates a single, overall set of performance
# metrics for both the Jussila and WiW models, treating all combined pixels as
# a single dataset for a final comparison.
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

#' Calculate performance metrics on an aggregated data frame for a specific model
#'
#' @param df A data frame containing a 'dominant_label' and a prediction column.
#' @param model_name A character string (e.g., "Jussila") to label the output.
#' @param prediction_col The name of the column containing the model's predictions.
#' @return A single-row data frame summarizing all performance metrics.
calculate_overall_performance <- function(df, model_name, prediction_col) {
  
  message(paste(" -> Calculating metrics for:", model_name, "(", nrow(df), "total pixels)"))
  
  # Map dominant labels to 'water', 'dry', or NA for evaluation
  evaluation_df <- df %>%
    mutate(reference_class = case_when(
      dominant_label == 'Inundated'     ~ 'water',
      dominant_label == 'Not inundated' ~ 'dry',
      TRUE                              ~ NA_character_
    )) %>%
    filter(!is.na(reference_class))
  
  message(paste("    Evaluating on", nrow(evaluation_df), "classified pixels (Inundated or Not inundated)"))
  
  # Calculate confusion matrix components using the specified prediction column
  tp_water <- sum(evaluation_df$reference_class == "water" & evaluation_df[[prediction_col]] == "water")
  fp_water <- sum(evaluation_df$reference_class == "dry"   & evaluation_df[[prediction_col]] == "water")
  fn_water <- sum(evaluation_df$reference_class == "water" & evaluation_df[[prediction_col]] == "dry")
  
  tp_dry <- sum(evaluation_df$reference_class == "dry"   & evaluation_df[[prediction_col]] == "dry")
  fp_dry <- sum(evaluation_df$reference_class == "water" & evaluation_df[[prediction_col]] == "dry")
  fn_dry <- sum(evaluation_df$reference_class == "dry"   & evaluation_df[[prediction_col]] == "water")
  
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
    Model = model_name,
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
# 2️⃣ Aggregate All Original Resolution Pixel Data
# ==============================================================================
message("--- Searching for and aggregating all ORIGINAL resolution pixel attribute files ---")

# Find all relevant CSV files
all_files <- list.files(
  path = pixel_data_root_dir,
  pattern = "_final_pixel_attributes.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Filter for ONLY original resolution files
original_files <- all_files[!grepl("_superres", all_files)]

if (length(original_files) == 0) {
  stop("FATAL: No 'original' resolution pixel attribute files found.")
}

message(paste("Found", length(original_files), "'original' resolution files to combine."))

# --- Load and combine all ORIGINAL files into a single data frame ---
all_original_pixels_df <- lapply(original_files, read.csv) %>%
  bind_rows() %>%
  # Standardize column names to lowercase for consistency
  rename_with(tolower)

# Check if the required prediction columns exist
required_cols <- c("predicted_jussila", "predicted_wiw")
if (!all(required_cols %in% names(all_original_pixels_df))) {
  stop("FATAL: The combined data frame is missing one or both prediction columns: 'predicted_jussila', 'predicted_wiw'. Please ensure the analysis scripts for both models have been run.")
}

# ==============================================================================
# 3️⃣ Calculate and Display Final Comparison Table
# ==============================================================================
message("\n--- Calculating overall performance across all combined data ---")

# Calculate performance for the Jussila model
jussila_performance <- calculate_overall_performance(
  df = all_original_pixels_df,
  model_name = "Jussila",
  prediction_col = "predicted_jussila"
)

# Calculate performance for the WiW model
wiw_performance <- calculate_overall_performance(
  df = all_original_pixels_df,
  model_name = "WiW",
  prediction_col = "predicted_wiw"
)

# --- Combine results into a final table ---
final_summary_table <- bind_rows(jussila_performance, wiw_performance)

message("\n--- Overall Performance Comparison: Jussila vs. WiW ---")
print(final_summary_table, width = Inf)

# --- Save the final summary to a CSV file ---
output_filename <- file.path(summary_output_dir, "jussila_vs_wiw_overall_performance.csv")
write.csv(final_summary_table, output_filename, row.names = FALSE)
message(paste("\nSUCCESS: Overall summary table saved to:\n", output_filename))

