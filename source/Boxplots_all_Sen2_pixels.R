# ==============================================================================
# Aggregate Boxplots for 'Pure' Pixels (All Sites Combined)
# ------------------------------------------------------------------------------
# Purpose:
# This script aggregates all 'final_pixel_attributes.csv' files for the
# original resolution. It then generates a single set of boxplots showing
# the distribution of specific spectral indices/bands for all 'pure' pixels,
# aggregated across all sites and years.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(purrr)   # For iterating over files
library(stringr) # For text manipulation
library(tidyr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
study_sites <- c("Webbekomsbroek", "Schulensmeer", "Kloosterbeemden", "Webbekomsbroek2")
years <- c(2020, 2021, 2023, 2024)

# --- Root Directory ---
output_root_dir <- "output"

# --- Output Path for New Plots ---
synthesis_plots_dir <- file.path(output_root_dir, "synthesis_plots")
dir.create(synthesis_plots_dir, showWarnings = FALSE, recursive = TRUE)

# --- Plotting Parameters ---
main_label_colors <- c(
  "Inundated" = "#4cd2de",
  "Not inundated" = "#dc5199",
  "Other" = "#86eb79",
  "Uncertain" = "#ff7f00",
  "Reed" = "#c49c02"
)
# Order for plot axis
manual_label_order <- rev(c('Inundated', 'Not inundated', 'Other', 'Reed', 'Uncertain'))

# ==============================================================================
# 1️⃣ Find and Load All Pixel Attribute Datasets
# ==============================================================================
message("\n--- Finding and loading all ORIGINAL resolution pixel attribute CSVs ---")

all_runs <- expand.grid(study_site = study_sites, year = years)

# --- Load all Pixel Attribute files (Original Resolution Only) ---
pixel_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_final_pixel_attributes.csv")
)
existing_pixel_files <- pixel_files[file.exists(pixel_files)]

if (length(existing_pixel_files) == 0) {
  stop("FATAL: No 'final_pixel_attributes.csv' files found.")
}

# Load all files, standardize names
aggregated_pixels_df <- map_dfr(existing_pixel_files, function(file) {
  read.csv(file) %>%
    rename_with(tolower) # Standardize column names
})
message("Successfully loaded ", length(existing_pixel_files), " pixel attribute files, totaling ",
        format(nrow(aggregated_pixels_df), big.mark = ","), " pixels.")

# ==============================================================================
# 2️⃣ Generate Aggregated Boxplot for Pure Pixels
# ==============================================================================
message("\n--- Generating aggregated boxplots for 'pure' pixels (all sites combined) ---")

# --- Prepare data for plotting ---
pure_pixels_df <- aggregated_pixels_df %>%
  filter(mixture_category == "pure")

if (nrow(pure_pixels_df) == 0) {
  message("No 'pure' pixels found across all datasets. Skipping boxplot generation.")
} else {
  
  # UPDATED: Define a fixed list of columns to plot
  cols_to_plot <- c(
    "b04_scaled",    # Band 4
    "b8a_scaled",    # Band 8a (NIR)
    "b11_scaled",    # Band 11
    "b12_scaled",    # Band 12
    "mndwi12"        # MNDWI using B12
  )
  
  # Check which of the desired columns actually exist in the data frame
  cols_exist <- cols_to_plot %in% names(pure_pixels_df)
  if (!all(cols_exist)) {
    warning("Missing expected columns: ", paste(cols_to_plot[!cols_exist], collapse = ", "))
  }
  # Use only the columns that were found
  cols_to_plot_final <- cols_to_plot[cols_exist]
  
  if (length(cols_to_plot_final) == 0) {
    stop("None of the requested columns to plot were found in the aggregated data.")
  }
  
  message("Plotting the following variables: ", paste(cols_to_plot_final, collapse = ", "))
  
  # Reshape data to long format for plotting
  plot_data_long <- pure_pixels_df %>%
    select(dominant_label, all_of(cols_to_plot_final)) %>%
    pivot_longer(
      cols = -dominant_label,
      names_to = "variable_name",
      values_to = "value"
    ) %>%
    filter(is.finite(value))
  
  # Set the factor level order for the plot
  plot_data_long$dominant_label <- factor(plot_data_long$dominant_label, levels = manual_label_order)
  
  # --- Create the plot ---
  boxplot_pure_aggregated <- ggplot(plot_data_long, aes(x = dominant_label, y = value, fill = dominant_label)) +
    geom_boxplot() +
    facet_wrap(~ variable_name, scales = "free_y") +
    scale_fill_manual(values = main_label_colors, name = "Dominant Label") +
    labs(
      title = "Distribution of Selected Indices for 'Pure' Pixels (Original Resolution)",
      subtitle = "Aggregated across all study sites and years",
      x = "Dominant Label",
      y = "Value"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Slanted text for labels
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  # Print the plot
  print(boxplot_pure_aggregated) 
  
  # --- Save the plot ---
  output_filename <- file.path(synthesis_plots_dir, "ALL_SITES_aggregated_pure_boxplots_selected_bands.png")
  ggsave(
    output_filename,
    plot = boxplot_pure_aggregated,
    width = 9, # Can be narrower with fewer plots
    height = 6, # Can be shorter
    dpi = 300,
    limitsize = FALSE
  )
  message("Aggregated boxplot for selected variables saved to:\n", output_filename)
}

