# ==============================================================================
# Visualize Performance Difference: Jussila vs. WiW Models
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all model performance metrics for the original
# resolution, calculates the difference in performance between the 'WiW' and
# 'Jussila' models, and generates horizontal boxplots to visualize this
# difference for each metric.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
metrics_root_dir <- "output/pixel_data_tables"
plot_output_dir <- "output/summary_plots"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1️⃣ Aggregate All Performance Data
# ==============================================================================
message("--- Searching for and aggregating all performance metric files ---")

# Find all metric CSV files for the original resolution
all_metric_files <- list.files(
  path = metrics_root_dir,
  pattern = "_model_performance_metrics.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Filter out any super-resolution files to focus only on the original resolution
metric_files <- all_metric_files[!grepl("_superres", all_metric_files)]

if (length(metric_files) < 2) {
  stop("FATAL: At least two performance metric files (Jussila and WiW) are needed for comparison.")
}

all_metrics_df <- lapply(metric_files, function(file_path) {
  data <- read.csv(file_path)
  path_parts <- str_split(file_path, "[/\\\\]")[[1]]
  year <- path_parts[length(path_parts) - 1]
  study_site <- path_parts[length(path_parts) - 2]
  
  # Determine the model type from the filename
  model <- if (grepl("_wiw_", basename(file_path))) "WiW" else "Jussila"
  
  data %>%
    mutate(
      study_site = study_site,
      year = as.integer(year),
      model = model
    )
}) %>%
  bind_rows()

message("Successfully loaded and combined ", length(metric_files), " metric files.")

# ==============================================================================
# 2️⃣ Calculate Performance Difference
# ==============================================================================
message("\n--- Calculating performance difference (WiW - Jussila) ---")

# Filter for the "Overall" results and reshape the data
performance_diff_df <- all_metrics_df %>%
  filter(mixture_category == "Overall") %>%
  mutate(site_year = paste(study_site, year)) %>%
  # Select all relevant metrics, including macro_f1
  select(site_year, model, starts_with("f1_"), starts_with("recall_"), starts_with("precision_"), macro_f1) %>%
  # Pivot to a long format
  pivot_longer(
    cols = -c(site_year, model),
    names_to = "metric_type",
    values_to = "score"
  ) %>%
  # Pivot wider to get 'Jussila' and 'WiW' scores in separate columns
  pivot_wider(
    names_from = model,
    values_from = score
  ) %>%
  # Calculate the difference; drop rows where one of the models is missing
  filter(!is.na(Jussila) & !is.na(WiW)) %>%
  mutate(performance_difference = WiW - Jussila) %>%
  # Clean up metric names for plotting
  mutate(
    metric_name_clean = str_replace(metric_type, "_", " "),
    metric_name_clean = str_to_title(metric_name_clean)
  )

message("Performance differences calculated. Final structure for plotting:")
print(head(performance_diff_df))

# ==============================================================================
# 3️⃣ Generate and Save the Horizontal Boxplot
# ==============================================================================
message("\n--- Generating performance difference boxplot ---")

difference_boxplot <- ggplot(performance_diff_df, aes(x = performance_difference, y = metric_name_clean)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +
  geom_jitter(width = 0, height = 0.2, alpha = 0.6, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Set detailed axis ticks with decimal formatting
  scale_x_continuous(
    breaks = seq(-0.5, 0.5, by = 0.05),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # UPDATED: Titles and labels for the new comparison
  labs(
    title = "Performance Difference: WiW Model vs. Jussila Model",
    subtitle = "Positive values indicate WiW performed better",
    x = "Performance Difference (WiW score - Jussila score)",
    y = "Performance Metric"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank()
  )

print(difference_boxplot)

# --- Save the plot to a file ---
# UPDATED: Filename for the new comparison
output_filename <- file.path(plot_output_dir, "jussila_vs_wiw_difference_boxplot.png")
ggsave(
  output_filename,
  plot = difference_boxplot,
  width = 10,
  height = 8,
  dpi = 300
)

message("\nSUCCESS: Difference boxplot saved to:\n", output_filename)

