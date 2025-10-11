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
 target_year <- 2020
# target_year <- 2021
# target_year <- 2023
# target_year <- 2024

 # Add a resolution identifier for managing filenames
 spatial_resolution <- "superres"
 
 # --- Root Directory ---
 output_root_dir <- "output"
 
 # --- Input File Paths (Aligned with previous script's output) ---
 # Path to the superres pixel attributes CSV file
 input_csv_path <- file.path(
   output_root_dir, "pixel_data_tables", study_site_name, target_year,
   paste0(study_site_name, "_", target_year, "_final_pixel_attributes_", spatial_resolution, ".csv")
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
 manual_label_order <- rev(manual_label_order) # Reverse the order here
 
 # Define the order for mixture categories
 mixture_category_order <- c("pure", "mixed", "very_mixed") # Order for stacking and legend
 mixture_category_order  <- rev(mixture_category_order ) # Reverse the order here
 
 # ==============================================================================
 # 1️⃣ Load and Prepare Data
 # ==============================================================================
 message("\n--- Loading and preparing pixel data for '", study_site_name, "' (", target_year, ") ---")
 
 if (!file.exists(input_csv_path)) {
   stop("FATAL: Input CSV file not found at: \n", input_csv_path,
        "\nPlease run the previous 'Pixel Analysis' script for the superres data first.")
 }
 pixel_data_df <- read.csv(input_csv_path)
 message("Successfully loaded ", nrow(pixel_data_df), " pixels.")
 
 # Standardize column names to lowercase for consistency
 names(pixel_data_df) <- tolower(names(pixel_data_df))
 message("Column names standardized to lowercase.")
 
 # Ensure dominant_label and mixture_category are factors with desired order for plotting
 pixel_data_df$dominant_label <- factor(pixel_data_df$dominant_label, levels = manual_label_order)
 
 # Factor mixture_category with the desired *stacking* order
 pixel_data_df$mixture_category <- factor(pixel_data_df$mixture_category, levels = mixture_category_order)
 
 
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
       subtitle = paste("Site:", study_site_name, "| Year:", target_year, "| Resolution:", spatial_resolution),
       x = "Dominant Label", y = "Value"
     ) +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   print(boxplot_pure)
   
   # Save the plot
   plot_output_dir <- file.path(output_root_dir, "plots", study_site_name, target_year)
   dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)
   # UPDATED: Add resolution to output filename
   ggsave(
     file.path(plot_output_dir, paste0("boxplot_pure_pixels_", spatial_resolution, ".png")),
     plot = boxplot_pure, width = 14, height = 10, dpi = 300
   )
   message("Boxplot for pure pixels saved.")
 } else {
   message("No 'pure' pixels found to generate boxplots.")
 }
 
 # --- 2.3 Generate Bar Plot: Pixel Counts by Label and Mixture Class (UPDATED FOR STACKED) -----
 message("Generating stacked bar plot for pixel counts by dominant label and mixture category...")
 
 # Calculate counts for the bar plot
 bar_plot_data <- pixel_data_df %>%
   count(dominant_label, mixture_category, name = "pixel_count") %>%
   complete(dominant_label, mixture_category, fill = list(pixel_count = 0))
 
 # Ensure mixture_category is a factor with the desired order for alpha mapping
 bar_plot_data$mixture_category <- factor(bar_plot_data$mixture_category, levels = mixture_category_order)
 
 bar_plot_counts <- ggplot(bar_plot_data, aes(y = dominant_label, x = pixel_count, fill = dominant_label, alpha = mixture_category)) +
   geom_bar(stat = "identity", position = "stack") +
   scale_fill_manual(
     name = "Dominant Label",
     values = main_label_colors,
     breaks = rev(manual_label_order),
     drop = FALSE
   ) +
   scale_alpha_manual(
     name = "Purity Class",
     values = c("very_mixed" = 0.3, "mixed" = 0.6, "pure" = 1.0),
     breaks = mixture_category_order,
     labels = c("Very Mixed (<60%)", "Mixed (60-90%)", "Pure (>90%)"),
     drop = FALSE
   ) +
   scale_x_continuous(labels = comma) +
   labs(
     title = "Pixel Counts by Dominant Label and Purity Class",
     subtitle = paste("Site:", study_site_name, "| Year:", target_year, "| Resolution:", spatial_resolution),
     y = "Dominant Label",
     x = "Number of Pixels"
   ) +
   theme_bw() +
   theme(
     axis.text.y = element_text(angle = 0, hjust = 1),
     plot.title = element_text(hjust = 0.5),
     plot.subtitle = element_text(hjust = 0.5),
     legend.position = "right",
     legend.box = "vertical",
     legend.title = element_text(face = "bold")
   )
 
 print(bar_plot_counts)
 
 # Save the horizontal stacked bar plot
 # Add resolution to output filename
 ggsave(
   file.path(plot_output_dir, paste0(study_site_name, "_", target_year, "_pixel_counts_stacked_bar_", spatial_resolution, ".png")),
   plot = bar_plot_counts, width = 12, height = 7, dpi = 300
 )
 message("Horizontal stacked bar plot for pixel counts saved.")
 
 # ==============================================================================
 # 3️⃣ Apply and Evaluate Decision Tree Model
 # ==============================================================================
 message("\n--- Applying and evaluating pre-trained decision tree model ---")
 
 # --- 3.1 Load the pre-trained model -------------------------------------------
 if (!file.exists(model_path)) stop("FATAL: Model file not found at: ", model_path)
 load(model_path, envir = .GlobalEnv) # Assumes model object is 'tree_jussila'
 message("Decision tree model loaded successfully.")
 
 # --- 3.2 Identify predictors and make predictions -----------------------------
 required_predictors <- attr(tree_jussila$terms, "term.labels")
 message("Model requires predictors: ", paste(required_predictors, collapse = ", "))
 
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
 evaluation_df <- pixel_data_df %>%
   mutate(reference_class = case_when(
     dominant_label == 'Inundated' ~ 'water',
     dominant_label == 'Not inundated'~ 'dry',
     TRUE ~ NA_character_
   )) %>%
   filter(!is.na(reference_class))
 
 performance_by_category <- evaluation_df %>%
   group_by(mixture_category) %>%
   summarise(
     tp_water = sum(reference_class == "water" & predicted_jussila == "water"),
     fp_water = sum(reference_class == "dry"   & predicted_jussila == "water"),
     fn_water = sum(reference_class == "water" & predicted_jussila == "dry"),
     tp_dry = sum(reference_class == "dry"   & predicted_jussila == "dry"),
     fp_dry = sum(reference_class == "water" & predicted_jussila == "dry"),
     fn_dry = sum(reference_class == "dry"   & predicted_jussila == "water"),
     .groups = 'drop'
   )
 
 performance_overall <- evaluation_df %>%
   summarise(
     tp_water = sum(reference_class == "water" & predicted_jussila == "water"),
     fp_water = sum(reference_class == "dry"   & predicted_jussila == "water"),
     fn_water = sum(reference_class == "water" & predicted_jussila == "dry"),
     tp_dry = sum(reference_class == "dry"   & predicted_jussila == "dry"),
     fp_dry = sum(reference_class == "water" & predicted_jussila == "dry"),
     fn_dry = sum(reference_class == "dry"   & predicted_jussila == "water")
   ) %>%
   mutate(mixture_category = "Overall")
 
 performance_summary <- bind_rows(performance_by_category, performance_overall) %>%
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
 
 message("\n--- Final Performance Metrics ---")
 print(performance_summary)
 
 # --- 3.5 Visualize Confusion Matrix as a Heatmap ------------------------------
 message("Generating a heatmap for the confusion matrix")
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
 
 custom_fill_colors <- c(
   "Correct_Prediction"   = "#22c55e",
   "Incorrect_Prediction" = "#ef4444",
   "Other_Category"       = "white"
 )
 
 confusion_plot <- ggplot(confusion_df, aes(x = Model_Prediction, y = Dominant_Label)) +
   geom_tile(aes(fill = Cell_Category), color = "black") +
   geom_text(aes(label = scales::comma(Count)), size = 5, color = "black") +
   scale_fill_manual(values = custom_fill_colors, guide = "none") +
   labs(
     title = "Confusion Matrix",
     subtitle = paste("Site:", study_site_name, "| Year:", target_year, "| Resolution:", spatial_resolution),
     x = "Model Prediction", y = "Dominant Label (Reference)"
   ) +
   theme_bw(base_size = 14) +
   theme(
     plot.title = element_text(hjust = 0.5, face = "bold"),
     plot.subtitle = element_text(hjust = 0.5),
     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
     legend.position = "none",
     axis.title = element_blank(),
     axis.ticks = element_blank(),
     panel.grid = element_blank(),
     panel.border = element_blank()
   )
 
 print(confusion_plot)
 
 # Add resolution to output filename
 ggsave(
   file.path(plot_output_dir, paste0("confusion_matrix_heatmap_", spatial_resolution, ".png")),
   plot = confusion_plot, width = 8, height = 7, dpi = 300
 )
 message("Confusion matrix heatmap saved.")
 
 # ==============================================================================
 # 4️⃣ Generate Final Performance Plots
 # ==============================================================================
 message("\n--- Generating final performance plots ---")
 
 # --- 4.1 Plot model performance metrics ---------------------------------------
 metrics_plot_data_full <- performance_summary %>%
   select(mixture_category, starts_with("recall"), starts_with("precision"), starts_with("f1")) %>%
   pivot_longer(
     -mixture_category,
     names_to = c("metric", "class"),
     names_pattern = "(.*)_(.*)",
     values_to = "value"
   )
 
 metrics_plot_data <- metrics_plot_data_full %>%
   filter(mixture_category != "Overall")
 
 metrics_plot_data$mixture_category <- factor(
   metrics_plot_data$mixture_category,
   levels = c("pure", "mixed", "very_mixed")
 )
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
     subtitle = paste("Site:", study_site_name, "| Year:", target_year, "| Resolution:", spatial_resolution),
     x = "Mixture Level", y = "Score"
   ) +
   theme_bw()
 
 print(metrics_plot)
 # UPDATED: Add resolution to output filename
 ggsave(
   file.path(plot_output_dir, paste0("metrics_line_plot_", spatial_resolution, ".png")),
   plot = metrics_plot, width = 10, height = 6, dpi = 300
 )
 message("Performance metrics plot saved.")
 
 # ==============================================================================
 # 5️⃣ Save Performance Metrics Output
 # ==============================================================================
 message("\n--- Saving performance metrics table ---")
 
 # --- 5.1 Define output path and save the CSV ----------------------------------
 # Add resolution to output filename
 metrics_csv_path <- file.path(
   output_root_dir, "pixel_data_tables", study_site_name, target_year,
   paste0(study_site_name, "_", target_year, "_model_performance_metrics_", spatial_resolution, ".csv")
 )
 dir.create(dirname(metrics_csv_path), recursive = TRUE, showWarnings = FALSE)
 write.csv(performance_summary, file = metrics_csv_path, row.names = FALSE)
 message("SUCCESS: Performance metrics data frame saved to:\n", metrics_csv_path)
 
 # --- 5.2 Overwrite the pixel attributes CSV with the new prediction column ---
 message("\nUpdating the original pixel attributes file with model predictions...")
 write.csv(pixel_data_df, file = input_csv_path, row.names = FALSE)
 message("SUCCESS: The file has been updated with the 'predicted_jussila' column:\n", input_csv_path)
 
 