# ==============================================================================
# Analysis and Modeling of Processed Pixel Data
# ------------------------------------------------------------------------------
# Purpose:
# This script loads a pre-processed CSV file containing pixel attributes.
# It performs data summarization, visualization (boxplots), applies a
# pre-trained decision tree model, and evaluates the model's performance.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart) # For the decision tree model
library(scales) # For formatting plot labels


# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
# study_site_name <- "Webbekomsbroek2"

# Select the single target year for this analysis run.
# target_year <- 2021 # Currently active year
# target_year <- 2020
 target_year <- 2023
# target_year <- 2024

# --- Root Directory ---
output_root_dir <- "output"

# --- Input File Paths (Aligned with previous script's output) ---
# Path to the final pixel attributes CSV file
input_csv_path <- file.path(
  output_root_dir, "pixel_data_tables", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_pixel_attributes.csv")
)

# Path to the pre-trained decision tree model
model_path <- "source/jussila_decisiontree.RData"


# --- Plotting Parameters (with "Reed" added) ---
# Define colors and order for plots to ensure consistency.
main_label_colors <- c(
  "Inundated" = "#4cd2de",
  "Not inundated" = "#dc5199",
  "Other" = "#86eb79",
  "Uncertain" = "#ff7f00",
  "Reed" = "#c49c02" # Added new color for Reed
)

# Define the desired order of labels on plot axes.
manual_label_order <- c('Inundated', 'Not inundated', 'Other', 'Reed', 'Uncertain')


# ==============================================================================
# 1️⃣ Load and Prepare Data
# ==============================================================================
message("\n--- Loading and preparing pixel data for '", study_site_name, "' (", target_year, ") ---")

if (!file.exists(input_csv_path)) {
  stop("FATAL: Input CSV file not found at: \n", input_csv_path, 
       "\nPlease run the 'run_Pixel_Analysis.R' script first.")
}
pixel_data_df <- read.csv(input_csv_path)
message("Successfully loaded ", nrow(pixel_data_df), " pixels.")

# Standardize column names to lowercase for consistency
names(pixel_data_df) <- tolower(names(pixel_data_df))
message("Column names standardized to lowercase.")


# ==============================================================================
# 2️⃣ Data Summary and Visualization
# ==============================================================================
message("\n--- Summarizing and visualizing pixel data ---")

# --- 2.1 Summarize pixel counts -----------------------------------------------
message("Summary of pixel counts by dominant label and mixture category:")
pivot_table <- pixel_data_df %>%
  count(dominant_label, mixture_category) %>%
  pivot_wider(names_from = mixture_category, values_from = n, values_fill = 0)
print(pivot_table)

# --- 2.2 Generate Boxplots for Pure Pixels ------------------------------------
message("Generating boxplots for 'pure' pixels...")
pure_pixels_df <- pixel_data_df %>% filter(mixture_category == "pure")

if (nrow(pure_pixels_df) > 0) {
  # Identify columns to plot (scaled bands and indices)
  cols_to_plot <- names(pure_pixels_df)[
    (endsWith(names(pure_pixels_df), "_scaled")) | 
      (names(pure_pixels_df) %in% c("ndvi", "ndwi_mf", "mndwi11", "mndwi12", "ndmi_gao11", "str1", "str2"))
  ]
  
  # Reshape data to long format for plotting
  plot_data_long <- pure_pixels_df %>%
    select(dominant_label, all_of(cols_to_plot)) %>%
    pivot_longer(-dominant_label, names_to = "variable_name", values_to = "value") %>%
    filter(is.finite(value))
  
  # Set the factor level order for the plot
  plot_data_long$dominant_label <- factor(plot_data_long$dominant_label, levels = manual_label_order)
  
  boxplot_pure <- ggplot(plot_data_long, aes(x = dominant_label, y = value, fill = dominant_label)) +
    geom_boxplot() +
    facet_wrap(~ variable_name, scales = "free_y") +
    scale_fill_manual(values = main_label_colors, name = "Dominant Label") +
    labs(
      title = "Distribution of Indices for 'Pure' Pixels",
      subtitle = paste("Site:", study_site_name, "| Year:", target_year),
      x = "Dominant Label", y = "Value"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(boxplot_pure)
  
  # Save the plot
  plot_output_dir <- file.path(output_root_dir, "boxplots", study_site_name, target_year)
  dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    file.path(plot_output_dir, "boxplot_pure_pixels.png"),
    plot = boxplot_pure, width = 14, height = 10, dpi = 300
  )
  message("Boxplot for pure pixels saved.")
} else {
  message("No 'pure' pixels found to generate boxplots.")
}


# ==============================================================================
# 3️⃣ Apply and Evaluate Decision Tree Model
# ==============================================================================
message("\n--- Applying and evaluating pre-trained decision tree model ---")

# --- 3.1 Load the pre-trained model -------------------------------------------
if (!file.exists(model_path)) stop("FATAL: Model file not found at: ", model_path)
load(model_path, envir = .GlobalEnv) # Assumes model object is 'tree_jussila'
message("Decision tree model loaded successfully.")

# --- 3.2 Identify predictors and make predictions -----------------------------
# Automatically get predictor names from the model object
required_predictors <- attr(tree_jussila$terms, "term.labels")
message("Model requires predictors: ", paste(required_predictors, collapse = ", "))

# Check if all required columns exist in the data frame
if (!all(required_predictors %in% names(pixel_data_df))) {
  stop("FATAL: Not all required predictor columns are present in the loaded data.")
}
data_for_prediction <- pixel_data_df[, required_predictors]
pixel_data_df$predicted_jussila <- predict(tree_jussila, newdata = data_for_prediction, type = "class")
message("Predictions generated and added as 'predicted_jussila' column.")

# --- 3.3 Create confusion matrix ----------------------------------------------
message("Comparison of Dominant Label vs. Model Prediction:")
confusion_matrix <- table(
  `Dominant Label` = pixel_data_df$dominant_label,
  `Model Prediction` = pixel_data_df$predicted_jussila
)
print(confusion_matrix)

# --- 3.4 Calculate performance metrics ----------------------------------------
message("Calculating performance metrics (Recall, Precision, F1-Score)...")
# Map dominant labels to 'water', 'dry', or NA for evaluation
evaluation_df <- pixel_data_df %>%
  mutate(reference_class = case_when(
    dominant_label == 'Inundated' ~ 'water',
    dominant_label == 'Not inundated'~ 'dry',
    TRUE ~ NA_character_ # "uncertain" and "Reed" will be ignored in this calculation
  )) %>%
  filter(!is.na(reference_class))

# Calculate TP, FP, FN by mixture level for both 'water' and 'dry' classes
performance_summary <- evaluation_df %>%
  group_by(mixture_category) %>%
  summarise(
    # Water metrics
    tp_water = sum(reference_class == "water" & predicted_jussila == "water"),
    fp_water = sum(reference_class == "dry"   & predicted_jussila == "water"),
    fn_water = sum(reference_class == "water" & predicted_jussila == "dry"),
    # Dry metrics
    tp_dry = sum(reference_class == "dry"   & predicted_jussila == "dry"),
    fp_dry = sum(reference_class == "water" & predicted_jussila == "dry"),
    fn_dry = sum(reference_class == "dry"   & predicted_jussila == "water"),
    .groups = 'drop'
  ) %>%
  # Calculate recall, precision, and F1 for both classes
  mutate(
    recall_water    = tp_water / (tp_water + fn_water),
    precision_water = tp_water / (tp_water + fp_water),
    f1_water        = 2 * (precision_water * recall_water) / (precision_water + recall_water),
    recall_dry      = tp_dry / (tp_dry + fn_dry),
    precision_dry   = tp_dry / (tp_dry + fp_dry),
    f1_dry          = 2 * (precision_dry * recall_dry) / (precision_dry + recall_dry)
  ) %>%
  # Replace any NaN (from 0/0) with 0
  mutate(across(where(is.numeric), ~replace_na(., 0)))

print(performance_summary)


# ==============================================================================
# 4️⃣ Generate Final Performance Plots
# ==============================================================================
message("\n--- Generating final performance plots ---")

# --- 4.1 Plot model performance metrics ---------------------------------------
metrics_plot_data <- performance_summary %>%
  select(mixture_category, starts_with("recall"), starts_with("precision"), starts_with("f1")) %>%
  pivot_longer(
    -mixture_category, 
    names_to = c("metric", "class"), 
    names_pattern = "(.*)_(.*)", 
    values_to = "value"
  )

metrics_plot_data$mixture_category <- factor(metrics_plot_data$mixture_category, levels = c("pure", "mixed", "very_mixed"))
metrics_plot_data$metric <- factor(metrics_plot_data$metric, levels = c("recall", "precision", "f1"))

metrics_plot <- ggplot(metrics_plot_data, aes(x = mixture_category, y = value, group = class, color = class)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = -1.5, size = 3.5, show.legend = FALSE) +
  facet_wrap(~ metric) +
  scale_color_manual(name = "Class", values = c("water" = "#4cd2de", "dry" = "#dc5199")) +
  coord_cartesian(ylim = c(0, 1.05)) +
  labs(
    title = "Model Performance Trend Across Mixture Levels",
    subtitle = paste("Site:", study_site_name, "| Year:", target_year),
    x = "Mixture Level", y = "Score"
  ) +
  theme_bw()

print(metrics_plot)
ggsave(
  file.path(plot_output_dir, "metrics_line_plot.png"),
  plot = metrics_plot, width = 10, height = 6, dpi = 300
)
message("Performance metrics plot saved.")

