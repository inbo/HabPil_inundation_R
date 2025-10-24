# ==============================================================================
# Compare Jussila vs. WiW Model Performance for Each Site/Year
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all 'Overall' performance metrics for the Jussila and
# WiW models at the original resolution. It then generates a separate dot plot
# for each metric to visually compare their performance for each study site
# and year combination.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
metrics_root_dir <- "output/pixel_data_tables"
plot_output_dir <- "output/summary_plots"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Plotting Parameters ---
# UPDATED: Define the colors for each model instead of resolution
plot_colors <- c(
  "Jussila" = "#0072B2", # A clear blue
  "WiW"     = "#009E73"  # A distinct green
)

# ==============================================================================
# 1️⃣ Aggregate All Performance Data
# ==============================================================================
message("--- Searching for and aggregating Jussila and WiW performance metric files ---")

# Find all metric CSV files in the directory
all_metric_files <- list.files(
  path = metrics_root_dir,
  pattern = "_model_performance_metrics.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# UPDATED: Filter for ONLY original resolution files
metric_files <- all_metric_files[!grepl("_superres", all_metric_files)]

if (length(metric_files) < 2) {
  stop("FATAL: Could not find performance files for both Jussila and WiW models.")
}

# Read each file, determine the model, and combine them into a single data frame
all_metrics_df <- lapply(metric_files, function(file_path) {
  data <- read.csv(file_path)
  path_parts <- str_split(file_path, "[/\\\\]")[[1]]
  year <- path_parts[length(path_parts) - 1]
  study_site <- path_parts[length(path_parts) - 2]
  
  # UPDATED: Determine the model type from the filename
  model <- if (grepl("_wiw_", basename(file_path))) "WiW" else "Jussila"
  
  data %>%
    mutate(
      study_site = study_site,
      year = as.integer(year),
      model = model
    )
}) %>%
  bind_rows()

message("Successfully loaded and combined ", length(metric_files), " metric files for Jussila and WiW models.")

# ==============================================================================
# 2️⃣ Prepare Data for Plotting
# ==============================================================================
message("\n--- Preparing aggregated data for plotting ---")

# Filter for the "Overall" results and reshape the data
plot_data_long <- all_metrics_df %>%
  filter(mixture_category == "Overall") %>%
  mutate(site_year = paste(study_site, year)) %>%
  # Select all relevant metrics, including macro_f1
  select(site_year, model, starts_with("f1_"), starts_with("recall_"), starts_with("precision_"), macro_f1) %>%
  pivot_longer(
    # UPDATED: Pivot on all columns except the new 'model' column
    cols = -c(site_year, model),
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
  
  current_metric_data <- plot_data_long %>%
    filter(metric_type == metric)
  
  clean_metric_name <- current_metric_data$metric_name_clean[1]
  
  message(paste(" -> Generating plot for:", clean_metric_name))
  
  # UPDATED: ggplot call now uses 'model' for color
  comparison_plot <- ggplot(current_metric_data, aes(x = site_year, y = score, color = model)) +
    geom_line(aes(group = site_year), color = "grey80", linewidth = 1) +
    geom_point(size = 4.5, alpha = 0.9) +
    # UPDATED: Legend title and colors are now for the models
    scale_color_manual(name = "Model", values = plot_colors) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    # UPDATED: Subtitle now refers to the model comparison
    labs(
      title = paste("Model Performance Comparison:", clean_metric_name),
      subtitle = "Comparing 'Overall' metric for Jussila vs. WiW models (Original Resolution)",
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
  

  output_filename <- file.path(plot_output_dir, paste0("jussila_vs_wiw_comparison_", metric, ".png"))
  
  ggsave(
    output_filename,
    plot = comparison_plot,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  message("    SUCCESS: Plot saved to ", output_filename)
}

message("\nAll comparison plots have been generated successfully.")

