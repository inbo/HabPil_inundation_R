# ==============================================================================
# Create Separate Plots for Each "Overall" Performance Metric
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all model performance metric CSV files, filters for
# the 'Overall' results, and generates a separate dot plot for each key
# performance metric (including Macro F1) to compare the 'original' vs.
# 'superres' workflows.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
metrics_root_dir <- "output/pixel_data_tables"
plot_output_dir <- "output/summary_plots"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Plotting Parameters ---
plot_colors <- c(
  "original" = "#0072B2", # A clear blue
  "superres" = "#D55E00"  # A distinct orange/red
)

# ==============================================================================
# 1️⃣ Aggregate All Performance Data
# ==============================================================================
message("--- Searching for and aggregating all performance metric files ---")

metric_files <- list.files(
  path = metrics_root_dir,
  pattern = "_model_performance_metrics.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(metric_files) == 0) {
  stop("FATAL: No performance metric CSV files found in '", metrics_root_dir, "'.")
}

all_metrics_df <- lapply(metric_files, function(file_path) {
  data <- read.csv(file_path)
  path_parts <- str_split(file_path, "[/\\\\]")[[1]]
  year <- path_parts[length(path_parts) - 1]
  study_site <- path_parts[length(path_parts) - 2]
  resolution <- if (grepl("_superres", basename(file_path))) "superres" else "original"
  
  data %>%
    mutate(
      study_site = study_site,
      year = as.integer(year),
      resolution = resolution
    )
}) %>%
  bind_rows()

message("Successfully loaded and combined ", length(metric_files), " metric files.")

# ==============================================================================
# 2️⃣ Prepare Data for Plotting
# ==============================================================================
message("\n--- Preparing aggregated data for plotting ---")

# Filter for the "Overall" results and reshape the data
plot_data_long <- all_metrics_df %>%
  filter(mixture_category == "Overall") %>%
  mutate(site_year = paste(study_site, year)) %>%
  # Select all relevant metrics, including macro_f1
  select(site_year, resolution, starts_with("f1_"), starts_with("recall_"), starts_with("precision_"), macro_f1) %>%
  pivot_longer(
    cols = -c(site_year, resolution),
    names_to = "metric_type",
    values_to = "score"
  ) %>%
  # Clean up metric names
  mutate(
    metric_name_clean = str_replace(metric_type, "_", " "),
    metric_name_clean = str_to_title(metric_name_clean)
  )

# Get a unique list of all metrics to plot
all_metrics <- unique(plot_data_long$metric_type)

message("Data prepared. The following metrics will be plotted individually:")
print(all_metrics)

# ==============================================================================
# 3️⃣ Generate and Save a Plot for Each Metric
# ==============================================================================
message("\n--- Generating a separate plot for each performance metric ---")

for (metric in all_metrics) {
  
  # Filter the data for the current metric in the loop
  current_metric_data <- plot_data_long %>%
    filter(metric_type == metric)
  
  # Get the clean, capitalized name for the plot title
  clean_metric_name <- current_metric_data$metric_name_clean[1]
  
  message(paste(" -> Generating plot for:", clean_metric_name))
  
  comparison_plot <- ggplot(current_metric_data, aes(x = site_year, y = score, color = resolution)) +
    geom_line(aes(group = site_year), color = "grey80", linewidth = 1) +
    geom_point(size = 4.5, alpha = 0.9) +
    scale_color_manual(name = "Resolution", values = plot_colors) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = paste("Model Performance Comparison:", clean_metric_name),
      subtitle = "Comparing 'Overall' metric for each site and year",
      x = "Study Site & Year",
      y = "Performance Score"
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "top"
    )
  
  # Define a unique filename for each plot
  output_filename <- file.path(plot_output_dir, paste0("performance_comparison_", metric, ".png"))
  
  ggsave(
    output_filename,
    plot = comparison_plot,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  message("    SUCCESS: Plot saved to ", output_filename)
}

message("\nAll plots have been generated successfully.")


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

