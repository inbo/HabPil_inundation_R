# ==============================================================================
# Visualize Performance Difference with Horizontal Boxplots
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all model performance metrics, calculates the
# difference in performance between 'superres' and 'original' resolutions,
# and generates horizontal boxplots to visualize the distribution of this
# difference for each metric across all study site/year combinations.
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

if (length(metric_files) < 2) {
  stop("FATAL: At least two performance metric files (original and superres) are needed for comparison.")
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
# 2️⃣ Calculate Performance Difference
# ==============================================================================
message("\n--- Calculating performance difference (superres - original) ---")

# Filter for the "Overall" results and reshape the data
performance_diff_df <- all_metrics_df %>%
  filter(mixture_category == "Overall") %>%
  mutate(site_year = paste(study_site, year)) %>%
  # Select all relevant metrics, including macro_f1
  select(site_year, resolution, starts_with("f1_"), starts_with("recall_"), starts_with("precision_"), macro_f1) %>%
  # Pivot to a long format
  pivot_longer(
    cols = -c(site_year, resolution),
    names_to = "metric_type",
    values_to = "score"
  ) %>%
  # Pivot wider to get 'original' and 'superres' scores in separate columns
  pivot_wider(
    names_from = resolution,
    values_from = score
  ) %>%
  # Calculate the difference; drop rows where one of the resolutions is missing
  filter(!is.na(original) & !is.na(superres)) %>%
  mutate(performance_difference = superres - original) %>%
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
  # Add the horizontal boxplots
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  
  # Add individual points with jitter to see the distribution
  geom_jitter(width = 0, height = 0.2, alpha = 0.6, color = "black") +
  
  # Add the critical zero line for reference
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Create more detailed axis ticks
  scale_x_continuous(
    breaks = seq(-0.3, 0.3, by = 0.02),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # Add titles and labels
  labs(
    title = "Performance Difference: Super-Resolution vs. Original",
    subtitle = "Positive values indicate superres performed better",
    x = "Performance Difference (superres score - original score)",
    y = "Performance Metric"
  ) +
  
  # Apply a clean theme
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank() # Remove horizontal grid lines for clarity
  )

print(difference_boxplot)

# --- Save the plot to a file ---
output_filename <- file.path(plot_output_dir, "performance_difference_boxplot.png")
ggsave(
  output_filename,
  plot = difference_boxplot,
  width = 10,
  height = 8,
  dpi = 300
)

message("\nSUCCESS: Difference boxplot saved to:\n", output_filename)
