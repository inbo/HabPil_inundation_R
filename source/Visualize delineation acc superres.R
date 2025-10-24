# ==============================================================================
# Visualize Delineation Metric Difference (IoU & Dice)
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all delineation metric CSV files, calculates the
# difference in IoU and Dice scores between 'SuperRes' and 'Original'
# resolutions, and generates horizontal boxplots to visualize the distribution
# of these differences.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
metrics_root_dir <- "output/pixel_data_tables"
plot_output_dir <- "output/summary_plots"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1️⃣ Aggregate All Delineation Metric Data
# ==============================================================================
message("--- Searching for and aggregating all delineation metric files ---")

# Find files named '_delineation_metrics.csv'
metric_files <- list.files(
  path = metrics_root_dir,
  pattern = "_delineation_metrics\\.csv$", # Exact match for the filename
  recursive = TRUE,
  full.names = TRUE
)

if (length(metric_files) == 0) {
  stop("FATAL: No '_delineation_metrics.csv' files found in '", metrics_root_dir, "'.")
}

# Read each file and combine them
# The files already contain study_site, year, and resolution
all_metrics_df <- map_dfr(metric_files, read.csv)

# Standardize resolution names if needed (e.g., from Original_10m to Original)
all_metrics_df <- all_metrics_df %>%
  mutate(resolution = case_when(
    grepl("Original", resolution, ignore.case = TRUE) ~ "Original",
    grepl("SuperRes", resolution, ignore.case = TRUE) ~ "SuperRes",
    TRUE ~ resolution # Keep as is if it doesn't match
  ))

message("Successfully loaded and combined ", length(metric_files), " delineation metric files.")
print(head(all_metrics_df))


# ==============================================================================
# 2️⃣ Calculate Performance Difference
# ==============================================================================
message("\n--- Calculating delineation metric difference (SuperRes - Original) ---")

# Reshape the data to calculate the difference
delineation_diff_df <- all_metrics_df %>%
  # Combine site and year for grouping
  mutate(site_year = paste(study_site, year)) %>%
  # Select relevant columns
  select(site_year, resolution, IoU, Dice_Coefficient) %>%
  # Pivot metrics to long format first
  pivot_longer(
    cols = c(IoU, Dice_Coefficient),
    names_to = "metric_type",
    values_to = "score"
  ) %>%
  # Pivot wider to get 'Original' and 'SuperRes' scores in separate columns
  pivot_wider(
    names_from = resolution,
    values_from = score
  ) %>%
  # Calculate the difference; drop rows where one resolution is missing
  filter(!is.na(Original) & !is.na(SuperRes)) %>%
  mutate(metric_difference = SuperRes - Original) %>%
  # Clean up metric names for plotting (add spaces if needed)
  mutate(
    metric_name_clean = gsub("_", " ", metric_type)
  )

message("Metric differences calculated. Final structure for plotting:")
print(head(delineation_diff_df))


# ==============================================================================
# 3️⃣ Generate and Save the Horizontal Boxplot
# ==============================================================================
message("\n--- Generating delineation metric difference boxplot ---")

difference_boxplot <- ggplot(delineation_diff_df, aes(x = metric_difference, y = metric_name_clean)) +
  # Add the horizontal boxplots
  geom_boxplot(fill = "skyblue", outlier.shape = NA) +
  
  # Add individual points with jitter
  geom_jitter(width = 0, height = 0.2, alpha = 0.7, color = "black") +
  
  # Add the critical zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  # Add detailed axis ticks with decimal formatting
  scale_x_continuous(
    breaks = seq(-0.5, 0.5, by = 0.05), # Adjust range/step if needed
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # Add titles and labels
  labs(
    title = "Delineation Metric Difference: Super-Resolution vs. Original",
    subtitle = "Positive values indicate superres had better spatial overlap with reference",
    x = "Metric Difference (SuperRes score - Original score)",
    y = "Delineation Metric"
  ) +
  
  # Apply a clean theme
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank()
  )

print(difference_boxplot)

# --- Save the plot to a file ---
output_filename <- file.path(plot_output_dir, "delineation_difference_boxplot.png")
ggsave(
  output_filename,
  plot = difference_boxplot,
  width = 10,
  height = 6, # Adjusted height slightly for two metrics
  dpi = 300
)

message("\nSUCCESS: Delineation difference boxplot saved to:\n", output_filename)
