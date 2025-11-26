# ==============================================================================
# Apply and Evaluate WiW Model on Processed Pixel Data
# ------------------------------------------------------------------------------
# Purpose:
# This script loads a pre-processed CSV file containing pixel attributes,
# applies the WiW decision rule (NIR & SWIR2 thresholds), and evaluates the
# model's performance against the reference labels.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
# --- Primary Analysis Parameters ---
# study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
 study_site_name <- "Webbekomsbroek2"

# Select the single target year for this analysis run.
# target_year <- 2020
# target_year <- 2021
# target_year <- 2023
 target_year <- 2024


# --- Root Directory ---
output_root_dir <- "output"

# --- Input File Paths ---
input_csv_path <- file.path(
  output_root_dir, "pixel_data_tables", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_pixel_attributes.csv")
)

# --- Plotting Parameters ---
manual_label_order <- rev(c('Inundated', 'Not inundated', 'Other', 'Reed', 'Uncertain'))
mixture_category_order <- rev(c("pure", "mixed", "very_mixed"))

# ==============================================================================
# 1️⃣ Load and Prepare Data
# ==============================================================================
message("\n--- Loading and preparing pixel data for '", study_site_name, "' (", target_year, ") ---")

if (!file.exists(input_csv_path)) {
  stop("FATAL: Input CSV file not found at: \n", input_csv_path)
}
pixel_data_df <- read.csv(input_csv_path)
message("Successfully loaded ", nrow(pixel_data_df), " pixels.")

# Standardize column names to lowercase for consistency
names(pixel_data_df) <- tolower(names(pixel_data_df))
message("Column names standardized to lowercase.")

# Ensure categorical columns are factors with the desired order
pixel_data_df$dominant_label <- factor(pixel_data_df$dominant_label, levels = manual_label_order)
pixel_data_df$mixture_category <- factor(pixel_data_df$mixture_category, levels = mixture_category_order)

# ==============================================================================
# 2️⃣ Apply and Evaluate WiW Model
# ==============================================================================
message("\n--- Applying and evaluating the WiW model ---")

# --- 2.1 Apply WiW decision rules ---
message("Applying WiW decision rules...")

# Check if required columns (NIR - B8A, SWIR2 - B12) exist
required_bands <- c("b8a_scaled", "b12_scaled")
if (!all(required_bands %in% names(pixel_data_df))) {
  stop("FATAL: The required columns 'b8a_scaled' and/or 'b12_scaled' were not found in the data.")
}

# The rule: water if NIR <= 0.1804 AND SWIR2 <= 0.1131, dry otherwise
pixel_data_df <- pixel_data_df %>%
  mutate(
    predicted_wiw = if_else(
      b8a_scaled <= 0.1804 & b12_scaled <= 0.1131,
      "water",
      "dry"
    )
  )
message("Predictions generated and added as 'predicted_wiw' column.")

# --- 2.2 Create confusion matrix ---
message("\nComparison of Dominant Label vs. WiW Model Prediction:")
confusion_matrix <- table(
  `Dominant Label` = pixel_data_df$dominant_label,
  `Model Prediction` = pixel_data_df$predicted_wiw
)
print(confusion_matrix)

# --- 2.3 Calculate performance metrics ---
message("\nCalculating performance metrics (Recall, Precision, F1-Score)...")
evaluation_df <- pixel_data_df %>%
  mutate(reference_class = case_when(
    dominant_label == 'Inundated' ~ 'water',
    dominant_label == 'Not inundated'~ 'dry',
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(reference_class))

# Calculate metrics per mixture category and overall
performance_summary <- evaluation_df %>%
  group_by(mixture_category) %>%
  summarise(
    tp_water = sum(reference_class == "water" & predicted_wiw == "water"),
    fp_water = sum(reference_class == "dry"   & predicted_wiw == "water"),
    fn_water = sum(reference_class == "water" & predicted_wiw == "dry"),
    tp_dry = sum(reference_class == "dry"   & predicted_wiw == "dry"),
    fp_dry = sum(reference_class == "water" & predicted_wiw == "dry"),
    fn_dry = sum(reference_class == "dry"   & predicted_wiw == "water"),
    .groups = 'drop'
  ) %>%
  bind_rows(
    evaluation_df %>%
      summarise(
        tp_water = sum(reference_class == "water" & predicted_wiw == "water"),
        fp_water = sum(reference_class == "dry"   & predicted_wiw == "water"),
        fn_water = sum(reference_class == "water" & predicted_wiw == "dry"),
        tp_dry = sum(reference_class == "dry"   & predicted_wiw == "dry"),
        fp_dry = sum(reference_class == "water" & predicted_wiw == "dry"),
        fn_dry = sum(reference_class == "dry"   & predicted_wiw == "water")
      ) %>%
      mutate(mixture_category = "Overall")
  ) %>%
  mutate(
    recall_water    = tp_water / (tp_water + fn_water),
    precision_water = tp_water / (tp_water + fp_water),
    f1_water        = 2 * (precision_water * recall_water) / (precision_water + recall_water),
    recall_dry      = tp_dry / (tp_dry + fn_dry),
    precision_dry   = tp_dry / (tp_dry + fp_dry),
    f1_dry          = 2 * (precision_dry * recall_dry) / (precision_dry + recall_dry)
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  mutate(macro_f1 = (f1_water + f1_dry) / 2)

message("\n--- Final WiW Model Performance Metrics ---")
print(performance_summary)

# ==============================================================================
# 3️⃣ Generate Performance Plots
# ==============================================================================
message("\n--- Generating final performance plots for WiW model ---")

plot_output_dir <- file.path(output_root_dir, "plots", study_site_name, target_year)
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# --- 3.1 Visualize Confusion Matrix as a Heatmap ---
confusion_df <- as.data.frame(confusion_matrix)
names(confusion_df) <- c("Dominant_Label", "Model_Prediction", "Count")

confusion_df <- confusion_df %>%
  mutate(
    Cell_Category = case_when(
      (Dominant_Label == "Inundated" & Model_Prediction == "water") ~ "Correct_Prediction",
      (Dominant_Label == "Not inundated" & Model_Prediction == "dry") ~ "Correct_Prediction",
      (Dominant_Label == "Inundated" & Model_Prediction == "dry") ~ "Incorrect_Prediction",
      (Dominant_Label == "Not inundated" & Model_Prediction == "water") ~ "Incorrect_Prediction",
      TRUE ~ "Other_Category"
    )
  )

custom_fill_colors <- c("Correct_Prediction" = "#22c55e", "Incorrect_Prediction" = "#ef4444", "Other_Category" = "white")

confusion_plot_wiw <- ggplot(confusion_df, aes(x = Model_Prediction, y = Dominant_Label)) +
  geom_tile(aes(fill = Cell_Category), color = "black") +
  geom_text(aes(label = scales::comma(Count)), size = 5, color = "black") +
  scale_fill_manual(values = custom_fill_colors, guide = "none") +
  labs(
    title = "WiW Model Confusion Matrix",
    # UPDATED: Removed resolution from subtitle
    subtitle = paste("Site:", study_site_name, "| Year:", target_year)
  ) +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = "none", axis.title = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), panel.border = element_blank())

print(confusion_plot_wiw)

ggsave(
  file.path(plot_output_dir, "confusion_matrix_heatmap_wiw.png"),
  plot = confusion_plot_wiw, width = 8, height = 7, dpi = 300
)
message("\nWiW confusion matrix heatmap saved.")

# --- 3.2 Plot model performance metrics trend ---
metrics_plot_data <- performance_summary %>%
  filter(mixture_category != "Overall") %>%
  select(mixture_category, starts_with("recall"), starts_with("precision"), starts_with("f1")) %>%
  pivot_longer(-mixture_category, names_to = c("metric", "class"), names_pattern = "(.*)_(.*)", values_to = "value")

metrics_plot_data$mixture_category <- factor(metrics_plot_data$mixture_category, levels = c("pure", "mixed", "very_mixed"))
metrics_plot_data$metric <- factor(metrics_plot_data$metric, levels = c("recall", "precision", "f1"))

metrics_plot_wiw <- ggplot(metrics_plot_data, aes(x = mixture_category, y = value, group = class, color = class)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = -1.5, size = 3.5, show.legend = FALSE) +
  facet_wrap(~ metric) +
  scale_color_manual(name = "Class", values = c("water" = "#4cd2de", "dry" = "#dc5199")) +
  coord_cartesian(ylim = c(0, 1.05)) +
  labs(
    title = "WiW Model Performance Trend Across Mixture Levels",
    # UPDATED: Removed resolution from subtitle
    subtitle = paste("Site:", study_site_name, "| Year:", target_year),
    x = "Mixture Level", y = "Score"
  ) +
  theme_bw()

print(metrics_plot_wiw)

ggsave(
  file.path(plot_output_dir, "metrics_line_plot_wiw.png"),
  plot = metrics_plot_wiw, width = 10, height = 6, dpi = 300
)
message("WiW performance metrics plot saved.")


# ==============================================================================
# 4️⃣ Save Performance Metrics Output
# ==============================================================================
message("\n--- Saving WiW performance metrics table ---")

metrics_csv_path <- file.path(
  output_root_dir, "pixel_data_tables", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_wiw_model_performance_metrics.csv")
)
write.csv(performance_summary, file = metrics_csv_path, row.names = FALSE)
message("SUCCESS: WiW performance metrics saved to:\n", metrics_csv_path)

# --- Overwrite the pixel attributes CSV with the new prediction column ---
message("\nUpdating the original pixel attributes file with WiW predictions...")
write.csv(pixel_data_df, file = input_csv_path, row.names = FALSE)
message("SUCCESS: The file has been updated with the 'predicted_wiw' column:\n", input_csv_path)

