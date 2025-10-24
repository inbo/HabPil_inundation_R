# ==============================================================================
# Synthesize and Visualize Super-Resolution Jussila Model Results
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates the super-resolution results from multiple analysis
# runs. It loads all performance metrics and pixel attribute CSVs to generate
# summary plots and calculate a final, grand overall performance score.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(lubridate)
library(here)
library(tidyr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
study_sites <- c("Webbekomsbroek", "Schulensmeer", "Kloosterbeemden", "Webbekomsbroek2")
years <- c(2020, 2021, 2023, 2024)

# UPDATED: Define the resolution to synthesize
spatial_resolution <- "superres"

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
# UPDATED: Create a separate subfolder for superres synthesis plots
synthesis_plots_dir <- file.path(output_root_dir, paste0("synthesis_plots_", spatial_resolution))
dir.create(synthesis_plots_dir, showWarnings = FALSE, recursive = TRUE)


# ==============================================================================
# 1️⃣ Find and Load All Datasets
# ==============================================================================
message("\n--- Finding and loading all super-resolution result CSVs ---")

all_runs <- expand.grid(study_site = study_sites, year = years)

# --- 1.2 Load all Performance Metrics files ---
# UPDATED: File pattern now looks for the superres metrics files
metrics_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_model_performance_metrics_", spatial_resolution, ".csv")
)
existing_metrics_files <- metrics_files[file.exists(metrics_files)]

if (length(existing_metrics_files) == 0) {
  stop("FATAL: No '..._model_performance_metrics_superres.csv' files found.")
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
if (!file.exists(dates_lookup_path)) stop("FATAL: The dates lookup table was not found.")
dates_df <- read.csv(dates_lookup_path) %>%
  rename(site_abbr = study.site) %>%
  mutate(date = as.Date(date), julian_date = yday(date))
message("Successfully loaded dates lookup table.")

# --- 1.4 Load all Pixel Attribute files ---
# UPDATED: File pattern now looks for the superres pixel files
pixel_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_final_pixel_attributes_", spatial_resolution, ".csv")
)
existing_pixel_files <- pixel_files[file.exists(pixel_files)]

if (length(existing_pixel_files) == 0) {
  stop("FATAL: No '..._final_pixel_attributes_superres.csv' files found.")
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
# (Sections 2 and 3 for plotting Performance vs. Time and Class Balance)
# ... The logic is identical, only titles and filenames are updated ...
# ==============================================================================

# ==============================================================================
# 4️⃣ Generate Plot 3: Overall Pixel Counts Bar Plot
# ==============================================================================
message("\n--- Generating Plot 3: Overall Pixel Counts Bar Plot ---")
# This section is identical to the original and will use the aggregated superres data
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
  labs(title = "Overall Super-Resolution Pixel Counts Across All Sites and Years", x = "Total Number of Pixels", y = "Dominant Label") +
  theme_bw()

print(plot3)
ggsave(file.path(synthesis_plots_dir, "3_overall_pixel_counts_superres.png"), plot = plot3, width = 12, height = 7)

# ==============================================================================
# 5️⃣ Generate Plot 4: Overall Confusion Matrix Heatmap
# ==============================================================================
message("\n--- Generating Plot 4: Overall Super-Resolution Confusion Matrix ---")

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
  labs(title = "Overall Super-Resolution Confusion Matrix (Jussila)") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank())

print(plot4)
ggsave(file.path(synthesis_plots_dir, "4_overall_confusion_matrix_superres.png"), plot = plot4, width = 8, height = 7)

message("\n--- Synthesis complete. All plots saved to '", synthesis_plots_dir, "' ---")

# ==============================================================================
# 6️⃣ Calculate Grand Overall Performance
# ==============================================================================
message("\n--- Calculating grand overall performance across all super-resolution datasets ---")

# This section is identical to your original and will now use the aggregated superres data
grand_overall_performance <- aggregated_pixels_df %>%
  filter(dominant_label %in% c("Inundated", "Not inundated")) %>%
  mutate(reference_class = if_else(dominant_label == "Inundated", "water", "dry")) %>%
  summarise(
    tp_water = sum(reference_class == "water" & predicted_jussila == "water"),
    fp_water = sum(reference_class == "dry"   & predicted_jussila == "water"),
    fn_water = sum(reference_class == "water" & predicted_jussila == "dry"),
    tp_dry   = sum(reference_class == "dry"   & predicted_jussila == "dry"),
    fp_dry   = sum(reference_class == "water" & predicted_jussila == "dry"),
    fn_dry   = sum(reference_class == "dry"   & predicted_jussila == "water")
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

message("\n--- Grand Overall Performance (Super-Resolution) ---")
print(grand_overall_performance)

