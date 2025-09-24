# ==============================================================================
# Apply Jussila Model and Create Comparison Visualization
# ------------------------------------------------------------------------------
# Purpose:
# This script applies the pre-trained Jussila decision tree model to an
# attributes raster to create a classified inundation map. It then generates
# two sets of three-panel plots to visually compare the reference labels
# against the model's predictions: one for all pixels and one filtered for
# "pure" pixels (based on mixture_category).
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(terra)
library(rpart)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggnewscale)


# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
# study_site_name <- "Webbekomsbroek2"

target_year <- 2023
# target_year <- 2020
# target_year <- 2021
# target_year <- 2024

# --- Root Directory ---
output_root_dir <- "output"

# --- Input File Paths ---
# Path to the raster containing all predictor layers (S2 bands, indices, labels)
attributes_raster_path <- file.path(
  output_root_dir, "final_rasters", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_attributes_raster.tif")
)

# Path to the pre-trained decision tree model
model_path <- "source/jussila_decisiontree.RData"

# --- Output File Paths ---
# Base directory for plot images
plots_output_dir <- file.path(output_root_dir, "plots", study_site_name, target_year)
dir.create(plots_output_dir, recursive = TRUE, showWarnings = FALSE) # Ensure directory exists

# Path for the classified map raster
classified_map_path <- file.path(
  output_root_dir, "classified_maps", study_site_name,
  paste0(study_site_name, "_", target_year, "_jussila_classified_map.tif")
)


# --- Plotting Parameters ---
# Colors for the reference map panel, with names capitalized to match the data.
reference_colors <- c(
  "Inundated" = "#4cd2de",
  "Not inundated" = "#dc5199",
  "Other" = "#86eb79",
  "Uncertain" = "#ff7f00",
  "Reed" = "#c49c02"
)

# Colors for the classified map panel
classified_colors <- c(
  "water" = "#4cd2de",
  "dry" = "#dc5199"
)

# Base colors for the detailed combination map (agreement/disagreement)
# Combinations with "NA" will be transparent, as they are not listed here.
combination_colors_base <- c(
  # Agreement
  "Inundated - water" = "#008080",      # Deep Teal (Perfect agreement for water)
  "Not inundated - dry" = "#8B4513",  # SaddleBrown (Perfect agreement for dry/land)
  "Other - dry" = "#6B8E23",          # OliveDrab (Other as dry)
  "Other - water" = "#1E90FF",        # DodgerBlue (Other as water)
  "Reed - dry" = "#8B7B00",            # Dark Goldenrod (Reed as dry)
  "Reed - water" = "#4682B4",          # SteelBlue (Reed as water)
  "Uncertain - dry" = "#CD853F",      # Peru (Uncertain as dry)
  "Uncertain - water" = "#5F9EA0",    # CadetBlue (Uncertain as water)
  
  # Disagreement / Misclassification (Errors)
  "Inundated - dry" = "#FF0000",        # Bright Red (Model MISSED Inundated water)
  "Not inundated - water" = "#FFA500"   # Orange (Model DETECTED water where it wasn't)
)


# ==============================================================================
# 1️⃣ Load Input Data (Raster and Model)
# ==============================================================================
message("\n--- Loading input data for '", study_site_name, "' (", target_year, ") ---")

if (!file.exists(attributes_raster_path)) {
  stop("FATAL: Attributes raster not found at: \n", attributes_raster_path)
}
attributes_raster <- rast(attributes_raster_path)
message("Attributes raster loaded.")


# --- Check for 'mixture_category' layer, essential for pure pixel filtering ---
if (!"mixture_category" %in% names(attributes_raster)) {
  stop("FATAL: 'mixture_category' layer not found in attributes_raster. Cannot filter for pure pixels.")
}
message("'mixture_category' layer found in attributes_raster.")

if (!file.exists(model_path)) {
  stop("FATAL: Model file not found at: ", model_path)
}
load(model_path, envir = .GlobalEnv) # Assumes model object is 'tree_jussila'
message("Decision tree model 'tree_jussila' loaded successfully.")


# ==============================================================================
# 2️⃣ Prepare Raster for Prediction
# ==============================================================================
message("\n--- Preparing raster by selecting and renaming predictor layers ---")

# --- Get the list of predictors the model needs ---
model_predictor_names <- attr(tree_jussila$terms, "term.labels")
message("Model requires predictors: ", paste(model_predictor_names, collapse = ", "))

# --- Rename S2 bands to lowercase to match model expectations ---
names(attributes_raster) <- tolower(names(attributes_raster))
message("Raster layer names standardized to lowercase.")

# --- Select only the layers required by the model ---
if (!all(model_predictor_names %in% names(attributes_raster))) {
  stop("FATAL: Not all required predictor layers were found in the attributes raster.")
}
predictor_stack <- subset(attributes_raster, model_predictor_names)
message("Predictor raster stack created.")


# ==============================================================================
# 3️⃣ Apply Model and Save Classified Map
# ==============================================================================
message("\n--- Applying model to generate and save classified map ---")
classified_map <- terra::predict(
  predictor_stack,
  tree_jussila,
  type = "class",
  na.rm = TRUE
)
message("Prediction complete.")

# --- Define and apply a color map for the classes ---
color_map <- data.frame(value = c(1, 2), color = c("#dc5199", "#4cd2de")) # dry, water
coltab(classified_map) <- color_map
message("Color map applied to the raster.")

# --- Save the classified raster to a GeoTIFF file ---
dir.create(dirname(classified_map_path), recursive = TRUE, showWarnings = FALSE)
writeRaster(
  classified_map, classified_map_path, overwrite = TRUE,
  wopt = list(gdal = c("COMPRESS=LZW"), datatype = "INT1U")
)
message("SUCCESS: Final classified map saved to:\n", classified_map_path)


# ==============================================================================
# 4️⃣ Prepare Base Data for Visualization
# ==============================================================================
message("\n--- Preparing base data for visualization ---")

# Step A: Extract layers
reference_map <- attributes_raster[["dominant_label"]]
mixture_category_map <- attributes_raster[["mixture_category"]] # Extract mixture_category layer

# Manually re-establish categories for reference map (critical for correct interpretation)
correct_ref_categories <- data.frame(
  value = c(1, 2, 3, 4, 5), # Numeric IDs as seen in QGIS
  category = c("Inundated", "Other", "Reed", "Uncertain", "Not inundated") # Corresponding text labels
)
terra::set.cats(reference_map, value = correct_ref_categories)

# Define explicit categories for mixture_category (based on your input: 1=pure, 2=mixed, 3=very mixed)
correct_mix_categories <- data.frame(
  value = as.integer(c(1, 2, 3)), # Numeric IDs
  category = c("pure", "mixed", "very mixed") # Corresponding text labels
)
terra::set.cats(mixture_category_map, value = correct_mix_categories)


# For reference_map: convert to data frame and ensure character type
ref_df <- terra::as.data.frame(reference_map, xy = TRUE, na.rm = FALSE)
names(ref_df)[3] <- "Reference"
ref_df$Reference <- as.character(ref_df$Reference)


# For classified_map: convert to data frame, get categories, and ensure character type
class_df <- terra::as.data.frame(classified_map, xy = TRUE, na.rm = FALSE)
names(class_df)[3] <- "class_pixel_value"
class_df$class_pixel_value <- as.integer(class_df$class_pixel_value)

class_cats <- terra::cats(classified_map)[[1]]
if (is.null(class_cats) || nrow(class_cats) == 0) {
  warning("Classified map has no defined categories. Using raw pixel values as labels for plotting.")
  class_df <- class_df %>%
    mutate(Classified = as.character(class_pixel_value)) %>%
    select(x, y, Classified)
} else {
  class_df <- class_df %>%
    left_join(class_cats, by = c("class_pixel_value" = "value")) %>%
    rename(Classified = class) %>%
    select(x, y, Classified)
}
class_df$Classified <- as.character(class_df$Classified)


# For mixture_category_map (NEW): convert to data frame and get categories
mixture_category_df <- terra::as.data.frame(mixture_category_map, xy = TRUE, na.rm = FALSE)
names(mixture_category_df)[3] <- "mixture_pixel_value" # Rename for internal processing

# Convert mixture_pixel_value to integer before joining
mixture_category_df$mixture_pixel_value <- as.integer(mixture_category_df$mixture_pixel_value)

# Define explicit categories for mixture_category, ensuring 'value' is integer
correct_mix_categories <- data.frame(
  value = as.integer(c(1, 2, 3)), # Explicitly make 'value' an integer
  category = c("pure", "mixed", "very mixed")
)

mixture_category_df <- mixture_category_df %>%
  left_join(correct_mix_categories, by = c("mixture_pixel_value" = "value")) %>%
  rename(MixtureCategory = category) %>%
  dplyr::select(x, y, MixtureCategory) # Select and rename final column


# Step C: Join all data frames by coordinate
base_comparison_df <- full_join(
  ref_df,
  class_df,
  by = c("x", "y")
) %>%
  full_join(mixture_category_df, by = c("x", "y")) %>% # Join mixture_category data
  # Filter out rows where *both* Reference and Classified are NA
  # These are areas with no information in either map, typically not plotted.
  filter(!(is.na(Reference) & is.na(Classified)))

message("Base comparison data frame prepared, including MixtureCategory values.")

# ==============================================================================
# 5️⃣ Plotting Function Definition
# ==============================================================================
# This function generates a three-panel ggplot for comparison.
plot_comparison_maps <- function(data_to_plot, title_suffix, filename_suffix) {
  message(paste0("\n--- Generating plot: ", title_suffix, " ---"))
  
  # Recalculate combination for the specific data_to_plot.
  # When paste() encounters NA, it converts it to the string "NA".
  current_plot_df_long <- data_to_plot %>%
    mutate(Combination = paste(Reference, "-", Classified)) %>%
    pivot_longer(
      cols = c("Reference", "Classified", "Combination"),
      names_to = "map_type",
      values_to = "label"
    ) %>%
    mutate(map_type = factor(map_type, levels = c("Reference", "Classified", "Combination")))
  
  # Determine all possible combinations labels in the current data for color mapping
  all_possible_combinations <- unique(current_plot_df_long$label[current_plot_df_long$map_type == "Combination"])
  
  # Initialize combination_colors for this plot run, starting with base colors
  current_combination_colors <- combination_colors_base
  
  # Identify combinations that are in the data but not in our base color list
  missing_combination_labels <- setdiff(all_possible_combinations, names(current_combination_colors))
  
  # Assign "transparent" to any missing combination labels (including "NA - ..." variants)
  if (length(missing_combination_labels) > 0) {
    fallback_colors <- setNames(rep("transparent", length(missing_combination_labels)), missing_combination_labels)
    current_combination_colors <- c(current_combination_colors, fallback_colors)
  }
  
  comparison_plot <- ggplot(data = current_plot_df_long, aes(x = x, y = y)) +
    geom_raster(data = . %>% filter(map_type == "Reference"), aes(fill = label)) +
    scale_fill_manual(name = "Reference Label", values = reference_colors, na.value = "white", drop = FALSE) +
    new_scale_fill() +
    geom_raster(data = . %>% filter(map_type == "Classified"), aes(fill = label)) +
    scale_fill_manual(name = "Classified Label", values = classified_colors, na.value = "white", drop = FALSE) +
    new_scale_fill() +
    geom_raster(data = . %>% filter(map_type == "Combination"), aes(fill = label)) +
    scale_fill_manual(name = "Agreement/Disagreement", values = current_combination_colors, na.value = "transparent", drop = FALSE) +
    facet_wrap(~ map_type) +
    labs(
      title = paste0("Comparison of Reference Labels vs. Classification Jussila Model (", title_suffix, ")"),
      subtitle = paste("Site:", study_site_name, "| Year:", target_year)
    ) +
    coord_equal() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      strip.text = element_text(face = "bold", size = 14),
      legend.position = "bottom", legend.box = "vertical",
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  print(comparison_plot)
  
  # --- Save the final plot ---
  plot_filename <- file.path(plots_output_dir, paste0(study_site_name, "_", target_year, "_", filename_suffix, ".png"))
  ggsave(
    filename = plot_filename, plot = comparison_plot,
    width = 15, height = 7, dpi = 300, bg = "white"
  )
  message("SUCCESS: Comparison map saved to:\n", plot_filename)
}


# ==============================================================================
# 6️⃣ Generate Plots
# ==============================================================================

# --- Plot 1: All Pixels ---
plot_comparison_maps(
  data_to_plot = base_comparison_df %>% dplyr::select(-MixtureCategory),
  title_suffix = "All Pixels",
  filename_suffix = "all_pixels_comparison_map_jussila_model"
)

# --- Plot 2: Pure Pixels Only ---
pure_pixels_df <- base_comparison_df %>%
  filter(MixtureCategory == "pure") %>% # Filter using the 'pure' label
  dplyr::select(-MixtureCategory) # Exclude MixtureCategory column from the final plot data

plot_comparison_maps(
  data_to_plot = pure_pixels_df,
  title_suffix = "Pure Pixels Only", # Updated title for clarity
  filename_suffix = "pure_pixels_comparison_map_jussila_model"
)

