# ==============================================================================
# Compare Jussila vs. WiW Model Predictions with a Confusion Matrix
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all 'final_pixel_attributes.csv' files for the
# original resolution and creates a confusion matrix to directly compare the
# 'water'/'dry' predictions of the Jussila model against the WiW model.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
pixel_data_root_dir <- "output/pixel_data_tables"
plot_output_dir <- "output/summary_plots"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1️⃣ Aggregate All Original Resolution Pixel Data
# ==============================================================================
message("--- Searching for and aggregating all ORIGINAL resolution pixel attribute files ---")

# Find all relevant CSV files
all_files <- list.files(
  path = pixel_data_root_dir,
  pattern = "_final_pixel_attributes.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Filter for ONLY original resolution files
original_files <- all_files[!grepl("_superres", all_files)]

if (length(original_files) == 0) {
  stop("FATAL: No 'original' resolution pixel attribute files found.")
}

message(paste("Found", length(original_files), "'original' resolution files to combine."))

# --- Load and combine all ORIGINAL files into a single data frame ---
all_original_pixels_df <- lapply(original_files, read.csv) %>%
  bind_rows() %>%
  rename_with(tolower)

# Check if the required prediction columns exist
required_cols <- c("predicted_jussila", "predicted_wiw")
if (!all(required_cols %in% names(all_original_pixels_df))) {
  stop("FATAL: The combined data is missing 'predicted_jussila' and/or 'predicted_wiw'. Please ensure the analysis scripts for both models have run.")
}

# ==============================================================================
# 2️⃣ Create and Visualize the Confusion Matrix
# ==============================================================================
message("\n--- Creating confusion matrix comparing Jussila vs. WiW predictions ---")

# Create the confusion matrix table
model_comparison_matrix <- table(
  `Jussila Prediction` = all_original_pixels_df$predicted_jussila,
  `WiW Prediction`     = all_original_pixels_df$predicted_wiw
)

message("Model Comparison Matrix:")
print(model_comparison_matrix)

# --- Convert the table to a data frame for ggplot ---
model_comparison_df <- as.data.frame(model_comparison_matrix)
names(model_comparison_df) <- c("Jussila_Prediction", "WiW_Prediction", "Pixel_Count")

# --- Create the heatmap plot ---
confusion_heatmap <- ggplot(model_comparison_df, aes(x = WiW_Prediction, y = Jussila_Prediction, fill = Pixel_Count)) +
  # Add the colored tiles
  geom_tile(color = "black") +
  
  # Add the count labels to the center of each tile
  geom_text(aes(label = scales::comma(Pixel_Count)), color = "white", size = 6, fontface = "bold") +
  
  # Use a color scale that is easy to read
  scale_fill_viridis_c(option = "cividis", labels = scales::comma) +
  
  # Add titles and labels
  labs(
    title = "Model Prediction Agreement: Jussila vs. WiW",
    subtitle = "Across all original resolution study sites and years",
    x = "WiW Model Prediction",
    y = "Jussila Model Prediction",
    fill = "Pixel Count"
  ) +
  
  # Apply a clean theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(face = "bold"),
    legend.position = "right"
  )

print(confusion_heatmap)

# --- Save the plot to a file ---
output_filename <- file.path(plot_output_dir, "jussila_vs_wiw_confusion_heatmap.png")
ggsave(
  output_filename,
  plot = confusion_heatmap,
  width = 10,
  height = 8,
  dpi = 300
)

message("\nSUCCESS: Comparison heatmap saved to:\n", output_filename)

