# ==============================================================================
# Synthesize and Visualize Results Across All Study Sites and Years
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates the results from multiple analysis runs. It loads all
# performance metrics and pixel attribute CSVs to generate summary plots that
# show performance trends over time, against class balance, and in overall
# summary visualizations.
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
# Maps full site names to the abbreviations used in 'sen2_dates.csv'
site_abbreviations <- list(
  "Webbekomsbroek"  = "WB",
  "Schulensmeer"    = "SM",
  "Kloosterbeemden" = "KB",
  "Webbekomsbroek2" = "WB2" 
)

# --- Root Directory ---
output_root_dir <- "output"

# --- Output Path for New Plots ---
synthesis_plots_dir <- file.path(output_root_dir, "synthesis_plots")
dir.create(synthesis_plots_dir, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# 1️⃣ Find and Load All Datasets
# ==============================================================================
message("\n--- Finding and loading all result CSVs ---")

# --- 1.1 Create a grid of all site/year combinations ---
all_runs <- expand.grid(study_site = study_sites, year = years)

# --- 1.2 Load all Performance Metrics files ---
metrics_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_model_performance_metrics.csv")
)
existing_metrics_files <- metrics_files[file.exists(metrics_files)]

if (length(existing_metrics_files) == 0) {
  stop("FATAL: No 'model_performance_metrics.csv' files found.")
}

aggregated_metrics_df <- map_dfr(existing_metrics_files, function(file) {
  parts <- str_split(file, "/")[[1]]
  site <- parts[length(parts) - 2]
  year <- as.integer(parts[length(parts) - 1])
  read.csv(file) %>% mutate(study_site = site, year = year, .before = 1)
})
message("Successfully loaded ", length(existing_metrics_files), " performance metrics files.")

# --- 1.3 Load the Dates Lookup Table ---
dates_lookup_path <- here("data", "lookup_tables", "sen2_dates.csv")
if (!file.exists(dates_lookup_path)) {
  stop("FATAL: The dates lookup table was not found at: ", dates_lookup_path)
}
# Assuming the CSV has columns 'study.site', 'year', 'date'
dates_df <- read.csv(dates_lookup_path) %>%
  rename(site_abbr = study.site) %>% # Standardize column name
  mutate(
    date = as.Date(date),
    julian_date = yday(date)
  )
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
    select(dominant_label, mixture_category, predicted_jussila) %>%
    mutate(study_site = site, year = year, .before = 1)
})
message("Successfully loaded ", length(existing_pixel_files), " pixel attribute files, totaling ",
        format(nrow(aggregated_pixels_df), big.mark = ","), " pixels.")


# ==============================================================================
# 2️⃣ Generate Plot 1: Performance vs. Time of Year (Separate Plots)
# ==============================================================================
message("\n--- Generating Plot 1: Performance vs. Time of Year (Separate Plots) ---")

# --- Prepare data by joining metrics with date information ---
plot1_data_wide <- aggregated_metrics_df %>%
  filter(mixture_category == "pure") %>%
  mutate(site_abbr = recode(study_site, !!!site_abbreviations)) %>%
  left_join(dates_df, by = c("site_abbr", "year")) %>%
  filter(!is.na(julian_date)) 

# --- Pivot data to long format ---
plot1_data_long <- plot1_data_wide %>%
  pivot_longer(
    cols = c(recall_water, recall_dry, precision_water, precision_dry, macro_f1),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  mutate(
    metric_name = factor(
      metric_name,
      levels = c("recall_water", "recall_dry", "precision_water", "precision_dry", "macro_f1"),
      labels = c("Recall (Water)", "Recall (Dry)", "Precision (Water)", "Precision (Dry)", "Macro F1-Score")
    )
  )

# --- Create and save a separate plot for each metric ---
# Get the unique metric names to iterate over
metric_names <- unique(plot1_data_long$metric_name)

# Use purrr::walk to loop through each metric, create a plot, and save it
purrr::walk(metric_names, function(current_metric) {
  
  message(paste("Generating plot for metric:", current_metric))
  
  # Filter data for the current metric
  plot_data <- plot1_data_long %>%
    filter(metric_name == current_metric)
  
  # Create the plot object for the single metric
  p <- ggplot(plot_data, aes(x = julian_date, y = metric_value, color = study_site)) +
    # FIX: Revert to using geom_jitter for colored dots
    geom_jitter(
      size = 4, 
      alpha = 0.8,
      width = 1.5, # Jitters points horizontally to prevent overlap
      height = 0
    ) +
    labs(
      title = "Jussila Model Performance vs. Time of Year (Pure Pixels Only)",
      subtitle = paste("Metric:", current_metric),
      x = "Day of Year (Julian Date)",
      y = "Performance Score",
      color = "Study Site"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
  
  # Create a clean filename
  filename_metric <- tolower(gsub("[ ()-]", "_", current_metric))
  filename <- paste0("1_perf_vs_time_", filename_metric, ".png")
  
  # Save the plot
  ggsave(
    file.path(synthesis_plots_dir, filename),
    plot = p,
    width = 10,
    height = 7
  )
})


# ==============================================================================
# 3️⃣ Generate Plot 2: Performance vs. Class Balance (Pure Pixels, Separate Plots)
# ==============================================================================
message("\n--- Generating Plot 2: Performance vs. Class Balance (Pure Pixels, Separate Plots) ---")

# Data preparation is the same
class_proportions <- aggregated_pixels_df %>%
  filter(mixture_category == "pure") %>% 
  filter(dominant_label %in% c("Inundated", "Not inundated")) %>%
  group_by(study_site, year) %>%
  summarise(prop_inundated = mean(dominant_label == "Inundated"), .groups = 'drop')

# Join proportions with performance metrics for PURE pixels
plot2_data_wide <- aggregated_metrics_df %>%
  filter(mixture_category == "pure") %>% # <-- CHANGED
  left_join(class_proportions, by = c("study_site", "year"))

# Pivot the data to a long format
plot2_data_long <- plot2_data_wide %>%
  pivot_longer(
    cols = c(recall_water, recall_dry, precision_water, precision_dry, macro_f1),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  mutate(
    metric_name = factor(
      metric_name,
      levels = c("recall_water", "recall_dry", "precision_water", "precision_dry", "macro_f1"),
      labels = c("Recall (Water)", "Recall (Dry)", "Precision (Water)", "Precision (Dry)", "Macro F1-Score")
    )
  )

# --- Create and save a separate plot for each metric ---
metric_names <- unique(plot2_data_long$metric_name)

purrr::walk(metric_names, function(current_metric) {
  
  message(paste("Generating plot for metric:", current_metric))
  
  plot_data <- plot2_data_long %>%
    filter(metric_name == current_metric)
  
  p <- ggplot(plot_data, aes(x = prop_inundated, y = metric_value, color = study_site)) +
    geom_point(size = 4, alpha = 0.8) +
    scale_x_continuous(labels = scales::percent) +
    labs(
      title = "Model Performance vs. Reference Class Balance (Pure Pixels Only)", # Updated title
      subtitle = paste("Metric:", current_metric),
      x = "Proportion of 'Inundated' Pixels in Reference Data",
      y = "Performance Score",
      color = "Study Site"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  print(p)
  
  filename_metric <- tolower(gsub("[ ()-]", "_", current_metric))
  filename <- paste0("2_perf_vs_balance_", filename_metric, ".png") # Updated filename
  
  ggsave(
    file.path(synthesis_plots_dir, filename),
    plot = p,
    width = 10,
    height = 7
  )
})

# ==============================================================================
# 4️⃣ Generate Plot 3: Overall Pixel Counts Bar Plot
# ==============================================================================
message("\n--- Generating Plot 3: Overall Pixel Counts Bar Plot ---")

main_label_colors <- c("Inundated"="#4cd2de", "Not inundated"="#dc5199", "Other"="#86eb79", "Uncertain"="#ff7f00", "Reed"="#c49c02")
manual_label_order <- rev(c('Inundated', 'Not inundated', 'Other', 'Reed', 'Uncertain'))
mixture_category_order <- rev(c("pure", "mixed", "very_mixed"))

aggregated_pixels_df$dominant_label <- factor(aggregated_pixels_df$dominant_label, levels = manual_label_order)
aggregated_pixels_df$mixture_category <- factor(aggregated_pixels_df$mixture_category, levels = mixture_category_order)

plot3_data <- aggregated_pixels_df %>%
  count(dominant_label, mixture_category, name = "pixel_count") %>%
  complete(dominant_label, mixture_category, fill = list(pixel_count = 0))

plot3 <- ggplot(plot3_data, aes(y = dominant_label, x = pixel_count, fill = dominant_label, alpha = mixture_category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(name = "Dominant Label", values = main_label_colors, drop = FALSE) +
  scale_alpha_manual(name = "Purity Class", values = c("very_mixed" = 0.3, "mixed" = 0.6, "pure" = 1.0),
                     labels = c("Very Mixed", "Mixed", "Pure"), drop = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Overall Pixel Counts Across All Sites and Years", x = "Total Number of Pixels", y = "Dominant Label") +
  theme_bw()

print(plot3)
ggsave(file.path(synthesis_plots_dir, "3_overall_pixel_counts.png"), plot = plot3, width = 12, height = 7)


# ==============================================================================
# 5️⃣ Generate Plot 4: Overall Confusion Matrix Heatmap
# ==============================================================================
message("\n--- Generating Plot 4: Overall Confusion Matrix ---")

overall_confusion_matrix <- table(
  `Dominant Label` = aggregated_pixels_df$dominant_label,
  `Model Prediction` = aggregated_pixels_df$predicted_jussila
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
  labs(title = "Overall Confusion Matrix Jussila Flanders") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank())

print(plot4)
ggsave(file.path(synthesis_plots_dir, "4_overall_confusion_matrix.png"), plot = plot4, width = 8, height = 7)

message("\n--- Synthesis complete. All plots saved to '", synthesis_plots_dir, "' ---")

# ==============================================================================
# 6️⃣ Calculate Grand Overall Performance
# ==============================================================================
message("\n--- Calculating grand overall performance across all datasets ---")

# Use the full aggregated pixel data frame for a final, single performance score
grand_overall_performance <- aggregated_pixels_df %>%
  # 1. Filter for the two classes of interest from the reference labels
  filter(dominant_label %in% c("Inundated", "Not inundated")) %>%
  # 2. Create the reference class for comparison
  mutate(reference_class = if_else(dominant_label == "Inundated", "water", "dry")) %>%
  # 3. Summarise TP, FP, FN across the entire dataset (no grouping)
  summarise(
    tp_water = sum(reference_class == "water" & predicted_jussila == "water"),
    fp_water = sum(reference_class == "dry"   & predicted_jussila == "water"),
    fn_water = sum(reference_class == "water" & predicted_jussila == "dry"),
    tp_dry   = sum(reference_class == "dry"   & predicted_jussila == "dry"),
    fp_dry   = sum(reference_class == "water" & predicted_jussila == "dry"),
    fn_dry   = sum(reference_class == "dry"   & predicted_jussila == "water")
  ) %>%
  # 4. Calculate final metrics from the summary counts
  mutate(
    recall_water    = tp_water / (tp_water + fn_water),
    precision_water = tp_water / (tp_water + fp_water),
    f1_water        = 2 * (precision_water * recall_water) / (precision_water + recall_water),
    recall_dry      = tp_dry / (tp_dry + fn_dry),
    precision_dry   = tp_dry / (tp_dry + fp_dry),
    f1_dry          = 2 * (precision_dry * recall_dry) / (precision_dry + recall_dry),
    macro_f1        = (f1_water + f1_dry) / 2
  ) %>%
  # Replace any NaN (from 0/0) with 0
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Print the final summary table to the console
print(grand_overall_performance)

