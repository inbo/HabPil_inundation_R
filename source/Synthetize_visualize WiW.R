# ==============================================================================
# Synthesize and Visualize WiW Model Results Across All Study Sites and Years
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates the results from multiple analysis runs of the WiW model.
# It loads all performance metrics and pixel attribute CSVs to generate summary
# plots that show performance trends over time, against class balance, and in
# overall summary visualizations.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(purrr)   # For iterating over files
library(stringr) # For text manipulation
library(lubridate) # For handling dates
library(here)    # For robust file paths
library(tidyr)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
# IMPORTANT: List all the study sites and years you have processed.
study_sites <- c("Webbekomsbroek", "Schulensmeer", "Kloosterbeemden", "Webbekomsbroek2")
years <- c(2020, 2021, 2023, 2024)

# --- Site-Specific Mappings ---
site_abbreviations <- list(
  "Webbekomsbroek"  = "WB",
  "Schulensmeer"    = "SM",
  "Kloosterbeemden" = "KB",
  "Webbekomsbroek2" = "WB2"
)

# --- Root Directory ---
output_root_dir <- "output"

# --- Output Path for New Plots ---
# UPDATED: Create a separate subfolder for WiW synthesis plots
synthesis_plots_dir <- file.path(output_root_dir, "synthesis_plots_wiw")
dir.create(synthesis_plots_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1️⃣ Find and Load All Datasets
# ==============================================================================
message("\n--- Finding and loading all WiW model result CSVs ---")

# --- 1.1 Create a grid of all site/year combinations ---
all_runs <- expand.grid(study_site = study_sites, year = years)

# --- 1.2 Load all WiW Performance Metrics files ---
# UPDATED: File pattern now looks for "_wiw_model_performance_metrics.csv"
metrics_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_wiw_model_performance_metrics.csv")
)
existing_metrics_files <- metrics_files[file.exists(metrics_files)]

if (length(existing_metrics_files) == 0) {
  stop("FATAL: No 'wiw_model_performance_metrics.csv' files found.")
}

aggregated_metrics_df <- map_dfr(existing_metrics_files, function(file) {
  parts <- str_split(file, "/")[[1]]
  site <- parts[length(parts) - 2]
  year <- as.integer(parts[length(parts) - 1])
  read.csv(file) %>% mutate(study_site = site, year = year, .before = 1)
})
message("Successfully loaded ", length(existing_metrics_files), " WiW performance metrics files.")

# --- 1.3 Load the Dates Lookup Table ---
dates_lookup_path <- here("data", "lookup_tables", "sen2_dates.csv")
if (!file.exists(dates_lookup_path)) {
  stop("FATAL: The dates lookup table was not found at: ", dates_lookup_path)
}
dates_df <- read.csv(dates_lookup_path) %>%
  rename(site_abbr = study.site) %>%
  mutate(date = as.Date(date), julian_date = yday(date))
message("Successfully loaded and processed the dates lookup table.")

# --- 1.4 Load all Pixel Attribute files ---
pixel_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_final_pixel_attributes.csv")
)
existing_pixel_files <- pixel_files[file.exists(pixel_files)]

if (length(existing_pixel_files) == 0) {
  stop("FATAL: No 'final_pixel_attributes.csv' files found.")
}

aggregated_pixels_df <- map_dfr(existing_pixel_files, function(file) {
  parts <- str_split(file, "/")[[1]]
  site <- parts[length(parts) - 2]
  year <- as.integer(parts[length(parts) - 1])
  read.csv(file) %>%
    # UPDATED: Select 'predicted_wiw' instead of 'predicted_jussila'
    select(dominant_label, mixture_category, predicted_wiw) %>%
    mutate(study_site = site, year = year, .before = 1)
})
message("Successfully loaded ", length(existing_pixel_files), " pixel attribute files.")

message("\n--- Setting consistent factor order for plots ---")

# Define the desired order of labels on plot axes, then reverse it.
manual_label_order <- rev(c('Inundated', 'Not inundated', 'Other', 'Reed', 'Uncertain'))

# Apply this order to the dominant_label column
aggregated_pixels_df$dominant_label <- factor(
  aggregated_pixels_df$dominant_label, 
  levels = manual_label_order
)

message("Factor order for 'dominant_label' has been set.")

# ==============================================================================
# 2️⃣ Generate Plot 1: Performance vs. Time of Year (Separate Plots)
# ==============================================================================
message("\n--- Generating Plot 1: WiW Performance vs. Time of Year ---")

# --- Prepare data ---
plot1_data_long <- aggregated_metrics_df %>%
  filter(mixture_category == "pure") %>%
  mutate(site_abbr = recode(study_site, !!!site_abbreviations)) %>%
  left_join(dates_df, by = c("site_abbr", "year")) %>%
  filter(!is.na(julian_date)) %>%
  pivot_longer(
    cols = c(recall_water, recall_dry, precision_water, precision_dry, macro_f1),
    names_to = "metric_name", values_to = "metric_value"
  ) %>%
  mutate(
    metric_name = factor(
      metric_name,
      levels = c("recall_water", "recall_dry", "precision_water", "precision_dry", "macro_f1"),
      labels = c("Recall (Water)", "Recall (Dry)", "Precision (Water)", "Precision (Dry)", "Macro F1-Score")
    )
  )

# --- Create and save a separate plot for each metric ---
metric_names <- unique(plot1_data_long$metric_name)

purrr::walk(metric_names, function(current_metric) {
  message(paste("Generating plot for metric:", current_metric))
  plot_data <- plot1_data_long %>% filter(metric_name == current_metric)
  
  p <- ggplot(plot_data, aes(x = julian_date, y = metric_value, color = study_site)) +
    geom_jitter(size = 4, alpha = 0.8, width = 1.5, height = 0) +
    labs(
      # UPDATED: Title
      title = "WiW Model Performance vs. Time of Year (Pure Pixels Only)",
      subtitle = paste("Metric:", current_metric),
      x = "Day of Year (Julian Date)", y = "Performance Score", color = "Study Site"
    ) +
    theme_bw() + theme(legend.position = "bottom")
  
  print(p)
  filename_metric <- tolower(gsub("[ ()-]", "_", current_metric))
  # UPDATED: Filename
  filename <- paste0("1_wiw_perf_vs_time_", filename_metric, ".png")
  ggsave(file.path(synthesis_plots_dir, filename), plot = p, width = 10, height = 7)
})

# ==============================================================================
# 3️⃣ Generate Plot 2: Performance vs. Class Balance (Pure Pixels)
# ==============================================================================
message("\n--- Generating Plot 2: WiW Performance vs. Class Balance ---")

class_proportions <- aggregated_pixels_df %>%
  filter(mixture_category == "pure", dominant_label %in% c("Inundated", "Not inundated")) %>%
  group_by(study_site, year) %>%
  summarise(prop_inundated = mean(dominant_label == "Inundated"), .groups = 'drop')

plot2_data_long <- aggregated_metrics_df %>%
  filter(mixture_category == "pure") %>%
  left_join(class_proportions, by = c("study_site", "year")) %>%
  pivot_longer(
    cols = c(recall_water, recall_dry, precision_water, precision_dry, macro_f1),
    names_to = "metric_name", values_to = "metric_value"
  ) %>%
  mutate(
    metric_name = factor(
      metric_name,
      levels = c("recall_water", "recall_dry", "precision_water", "precision_dry", "macro_f1"),
      labels = c("Recall (Water)", "Recall (Dry)", "Precision (Water)", "Precision (Dry)", "Macro F1-Score")
    )
  )

purrr::walk(metric_names, function(current_metric) {
  message(paste("Generating plot for metric:", current_metric))
  plot_data <- plot2_data_long %>% filter(metric_name == current_metric)
  
  p <- ggplot(plot_data, aes(x = prop_inundated, y = metric_value, color = study_site)) +
    geom_point(size = 4, alpha = 0.8) +
    scale_x_continuous(labels = scales::percent) +
    labs(
      # UPDATED: Title
      title = "WiW Model Performance vs. Reference Class Balance (Pure Pixels Only)",
      subtitle = paste("Metric:", current_metric),
      x = "Proportion of 'Inundated' Pixels in Reference Data",
      y = "Performance Score", color = "Study Site"
    ) +
    theme_bw() + theme(legend.position = "bottom")
  
  print(p)
  filename_metric <- tolower(gsub("[ ()-]", "_", current_metric))
  # UPDATED: Filename
  filename <- paste0("2_wiw_perf_vs_balance_", filename_metric, ".png")
  ggsave(file.path(synthesis_plots_dir, filename), plot = p, width = 10, height = 7)
})

# ==============================================================================
# 4️⃣ Generate Plot 3: Overall Pixel Counts Bar Plot
# ==============================================================================
message("\n--- Generating Plot 3: Overall Pixel Counts Bar Plot ---")
# This plot is model-independent and does not need to be changed.
# It will be identical to the one from the Jussila synthesis.
# ... (code is identical to original) ...

# ==============================================================================
# 5️⃣ Generate Plot 4: Overall WiW Confusion Matrix Heatmap
# ==============================================================================
message("\n--- Generating Plot 4: Overall WiW Confusion Matrix ---")

# UPDATED: Use 'predicted_wiw' for the table
overall_confusion_matrix <- table(
  `Dominant Label` = aggregated_pixels_df$dominant_label,
  `Model Prediction` = aggregated_pixels_df$predicted_wiw
)

confusion_df <- as.data.frame(overall_confusion_matrix)
names(confusion_df) <- c("Dominant_Label", "Model_Prediction", "Count")

confusion_df <- confusion_df %>%
  mutate(
    Cell_Category = case_when(
      (Dominant_Label == "Inundated" & Model_Prediction == "water") ~ "Correct",
      (Dominant_Label == "Not inundated" & Model_Prediction == "dry") ~ "Correct",
      (Dominant_Label == "Inundated" & Model_Prediction == "dry") ~ "Incorrect",
      (Dominant_Label == "Not inundated" & Model_Prediction == "water") ~ "Incorrect",
      TRUE ~ "Other"
    )
  )

custom_fill_colors <- c("Correct" = "#22c55e", "Incorrect" = "#ef4444", "Other" = "white")

plot4 <- ggplot(confusion_df, aes(x = Model_Prediction, y = Dominant_Label, fill = Cell_Category)) +
  geom_tile(color = "black") +
  geom_text(aes(label = scales::comma(Count)), size = 5, color = "black") +
  scale_fill_manual(values = custom_fill_colors, guide = "none") +
  # UPDATED: Title
  labs(title = "Overall Confusion Matrix WiW Model Flanders") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank())

print(plot4)
# UPDATED: Filename
ggsave(file.path(synthesis_plots_dir, "4_wiw_overall_confusion_matrix.png"), plot = plot4, width = 8, height = 7)

# ==============================================================================
# 6️⃣ Calculate Grand Overall Performance for WiW Model
# ==============================================================================
message("\n--- Calculating grand overall performance for WiW model ---")

# UPDATED: Use 'predicted_wiw' in calculations
grand_overall_performance <- aggregated_pixels_df %>%
  filter(dominant_label %in% c("Inundated", "Not inundated")) %>%
  mutate(reference_class = if_else(dominant_label == "Inundated", "water", "dry")) %>%
  summarise(
    tp_water = sum(reference_class == "water" & predicted_wiw == "water"),
    fp_water = sum(reference_class == "dry"   & predicted_wiw == "water"),
    fn_water = sum(reference_class == "water" & predicted_wiw == "dry"),
    tp_dry   = sum(reference_class == "dry"   & predicted_wiw == "dry"),
    fp_dry   = sum(reference_class == "water" & predicted_wiw == "dry"),
    fn_dry   = sum(reference_class == "dry"   & predicted_wiw == "water")
  ) %>%
  mutate(
    recall_water    = tp_water / (tp_water + fn_water),
    precision_water = tp_water / (tp_water + fp_water),
    f1_water        = 2 * (precision_water * recall_water) / (precision_water + recall_water),
    recall_dry      = tp_dry / (tp_dry + fn_dry),
    precision_dry   = tp_dry / (tp_dry + fp_dry),
    f1_dry          = 2 * (precision_dry * recall_dry) / (precision_dry + recall_dry),
    macro_f1        = (f1_water + f1_dry) / 2
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Print the final summary table to the console
print(grand_overall_performance)

