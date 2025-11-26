# ==============================================================================
# Train (on Pure 2020/21/24) and Validate (on All 2023) Single-Index Trees
# ------------------------------------------------------------------------------
# Purpose:
# This script loads all original-resolution pixel data. It trains a decision
# tree on 'pure' pixels from 2020, 2021, & 2024, then validates that model
# against all pixels from 2023.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(purrr)   # For iterating over files
library(stringr) # For text manipulation
library(tidyr)
library(rpart)      # For training the decision tree
library(rpart.plot) # For plotting the tree
library(caret)      # For the confusion matrix
library(gridExtra)
library(ggplot2)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
study_sites <- c("Webbekomsbroek", "Schulensmeer", "Kloosterbeemden", "Webbekomsbroek2")
years <- c(2020, 2021, 2023, 2024) # All years to be loaded

# --- Define the list of predictors (indices) to test ---
indices_to_test <- c(
  "ndwi_mf",  # NDWI (B3, B8a)
  "mndwi11",  # mNDWI (B3, B11)
  "mndwi12"   # mNDWI (B3, B12)
)

# --- Root Directories ---
pixel_data_root_dir <- "output/pixel_data_tables"
output_plot_dir <- "output/synthesis_plots"
dir.create(output_plot_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1️⃣ Find and Load All Pixel Attribute Datasets
# ==============================================================================
message("\n--- Finding and loading all ORIGINAL resolution pixel attribute CSVs ---")

all_runs <- expand.grid(study_site = study_sites, year = years)

pixel_files <- file.path(
  pixel_data_root_dir, all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_final_pixel_attributes.csv")
)
existing_pixel_files <- pixel_files[file.exists(pixel_files)]

if (length(existing_pixel_files) == 0) {
  stop("FATAL: No 'final_pixel_attributes.csv' files found.")
}

# UPDATED: Load all files and keep 'year' column
aggregated_pixels_df <- map_dfr(existing_pixel_files, function(file) {
  # Extract year from the file path
  parts <- str_split(file, "[/\\\\]")[[1]]
  year_val <- as.integer(parts[length(parts) - 1])
  
  read.csv(file) %>%
    rename_with(tolower) %>% # Standardize column names
    mutate(year = year_val, .before = 1) # Add the year column
})

message("Successfully loaded ", length(existing_pixel_files), " pixel attribute files.")

# ==============================================================================
# 2️⃣ Prepare Data for Modeling
# ==============================================================================
message("\n--- Preparing data for modeling (Train on 2020/21/24 Pure, Validate on 2023 All) ---")

# 1. Create the base dataset: all 'Inundated'/'Not inundated' pixels
model_data_all <- aggregated_pixels_df %>%
  filter(dominant_label %in% c("Inundated", "Not inundated")) %>%
  mutate(
    class = factor(if_else(dominant_label == "Inundated", "water", "dry"))
  ) %>%
  # Select all columns needed for training and validation
  select(year, class, mixture_category, all_of(indices_to_test)) %>%
  na.omit()

# 2. Create the TRAINING set: ONLY 'pure' pixels from 2020, 2021, 2024
train_data <- model_data_all %>%
  filter(year %in% c(2020, 2021, 2024) & mixture_category == "pure") %>%
  select(-mixture_category, -year) # Remove helper columns

# 3. Create the VALIDATION set: ALL pixels from 2023
validation_data <- model_data_all %>%
  filter(year == 2023) %>%
  select(-mixture_category, -year) # Remove helper columns

# Check if data sets are empty
if(nrow(train_data) == 0) stop("FATAL: No 'pure' pixels found for training years (2020, 2021, 2024).")
if(nrow(validation_data) == 0) stop("FATAL: No 'Inundated' or 'Not inundated' pixels found for validation year (2023).")

message(paste("Created TRAINING dataset with", nrow(train_data), "'pure' pixels from 2020/21/24."))
print(table(train_data$class))
message(paste("Created VALIDATION dataset with", nrow(validation_data), "total pixels from 2023."))
print(table(validation_data$class))

# ==============================================================================
# 3️⃣ Loop, Train, and Validate Model for Each Index
# ==============================================================================

# Create an empty list to store the results
all_model_stats <- list()

for (index_name in indices_to_test) {
  
  message(paste("\n--- Training and validating model for:", index_name, "---"))
  
  # 1. Create the formula
  formula <- as.formula(paste("class ~", index_name))
  
  # 2. Train the decision tree on PURE data from 2020/21/24
  tree_model <- rpart(
    formula,
    data = train_data, # Use the "pure" training data
    method = "class",
    parms = list(prior = c(1/2, 1/2)), # Balance classes
    control = rpart.control(xval = 5)  # Use cross-validation
  )
  
  # 3. Plot the simple tree
  plot_title <- paste("Decision Tree for", index_name, "(Trained on Pure 2020/21/24)")
  plot_file <- file.path(output_plot_dir, paste0("tree_plot_pure_train_2023val_", index_name, ".png"))
  png(plot_file, width = 800, height = 600)
  rpart.plot(tree_model, main = plot_title, extra = 101, roundint = FALSE)
  dev.off()
  message(paste("Tree plot saved to:", plot_file))
  
  # --- Extract Threshold Value ---
  threshold_value <- NA
  if (!is.null(tree_model$splits) && nrow(tree_model$splits) > 0) {
    threshold_value <- tree_model$splits[1, "index"]
    split_direction <- if(tree_model$splits[1, "ncat"] == 1) "<" else ">"
    message(paste("  Found threshold:", index_name, split_direction, round(threshold_value, 4)))
  } else {
    message("  No split found. The model is a stump (predicts only one class).")
  }
  
  # 4. Make predictions on the FULL validation data from 2023
  predictions <- predict(tree_model, newdata = validation_data, type = "class")
  
  # 5. Generate and print the confusion matrix
  message(paste("Validation results for:", index_name, "(tested on all 2023 pixels)"))
  conf_matrix <- confusionMatrix(
    data = predictions,
    reference = validation_data$class,
    positive = "water" # Set 'water' as the positive class
  )
  
  print(conf_matrix)
  
  f1_water <- conf_matrix$byClass['F1']
  recall_dry <- conf_matrix$byClass['Specificity']
  precision_dry <- conf_matrix$byClass['Neg Pred Value']
  f1_dry <- (2 * (precision_dry * recall_dry) / (precision_dry + recall_dry))
  
  # Handle potential NA/NaN if one class has 0 predictions/references
  if(is.na(f1_water)) f1_water <- 0
  if(is.na(f1_dry)) f1_dry <- 0
  
  macro_f1_score <- (f1_water + f1_dry) / 2
  
  # 6. Store the key statistics
  all_model_stats[[index_name]] <- data.frame(
    Index = index_name,
    Threshold = threshold_value,
    Accuracy = conf_matrix$overall['Accuracy'],
    Kappa = conf_matrix$overall['Kappa'],
    F1_Water = f1_water,
    F1_Dry = f1_dry,
    macro_f1 = macro_f1_score,
    Sensitivity_Water = conf_matrix$byClass['Sensitivity'],
    Pos_Pred_Value_Water = conf_matrix$byClass['Pos Pred Value']
  )
}

# ==============================================================================
# 4️⃣ Show Final Summary
# ==============================================================================
message("\n--- Final Summary of All Single-Index Models (Trained on Pure 2020/21/24, Validated on 2023) ---")

# Combine all results into a single data frame and print
final_summary <- bind_rows(all_model_stats)
print(final_summary)

# Save the summary to a CSV
summary_csv_path <- file.path(output_plot_dir, "single_index_model_summary_2023val.csv")
write.csv(final_summary, summary_csv_path, row.names = FALSE)
message(paste("\nFull summary saved to:", summary_csv_path))

# --- Create and Display the Requested Summary Table ---
message("\n--- Summary Table (Index, Threshold, Macro F1) ---")

simple_summary_table <- final_summary %>%
  select(Index, Threshold, macro_f1) %>%
  # Round for clarity
  mutate(
    Threshold = round(Threshold, 4),
    macro_f1 = round(macro_f1, 4)
  )

print(simple_summary_table)

# --- Save Summary Table as PNG Image ---
message("\n--- Saving summary table as PNG ---")

# Define the output path for the PNG
table_png_path <- file.path(output_plot_dir, "single_index_model_threshold_f1_summary.png")

# Create the table graphical object (grob)
# This uses the 'simple_summary_table' which ONLY has Index, Threshold, and macro_f1
table_grob <- gridExtra::tableGrob(simple_summary_table, theme = ttheme_minimal())

# Save the table grob as a PNG file
ggsave(
  filename = table_png_path,
  plot = table_grob,
  width = 6,  # Adjust width as needed
  height = 3, # Adjust height as needed
  dpi = 300
)

message(paste("\nSummary table PNG saved to:", table_png_path))

