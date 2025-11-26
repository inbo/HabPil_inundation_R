# ==============================================================================
# Apply WiW Model and Create Comparison Visualization
# ------------------------------------------------------------------------------
# Purpose:
# This script applies the WiW decision rule (NIR ≤ 0.1804 and SWIR2 ≤ 0.1131)
# to an attributes raster to create a classified inundation map. It then
# generates comparison plots to visually evaluate the model's predictions
# against the reference labels.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(terra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggnewscale)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Primary Analysis Parameters ---
# study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
study_site_name <- "Webbekomsbroek2"

# target_year <- 2020
# target_year <- 2021
# target_year <- 2023
target_year <- 2024

# --- Root Directory ---
output_root_dir <- "output"

# --- Input File Paths ---
# Path to the raster containing all predictor layers (S2 bands, indices, labels)
attributes_raster_path <- file.path(
  output_root_dir, "final_rasters", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_attributes_raster.tif")
)

# --- Output File Paths ---
# UPDATED: Paths now refer to 'wiw' instead of 'jussila'
plots_output_dir <- file.path(output_root_dir, "plots", study_site_name, target_year)
dir.create(plots_output_dir, recursive = TRUE, showWarnings = FALSE)

classified_map_path <- file.path(
  output_root_dir, "classified_maps", study_site_name,
  paste0(study_site_name, "_", target_year, "_wiw_classified_map.tif")
)


# --- Plotting Parameters ---
# (Colors remain the same)
reference_colors <- c(
  "Inundated" = "#4cd2de",
  "Not inundated" = "#dc5199",
  "Other" = "#86eb79",
  "Uncertain" = "#ff7f00",
  "Reed" = "#c49c02"
)
classified_colors <- c(
  "water" = "#4cd2de",
  "dry" = "#dc5199"
)
combination_colors_base <- c(
  "Inundated - water" = "#008080", "Not inundated - dry" = "#8B4513",
  "Other - dry" = "#6B8E23", "Other - water" = "#1E90FF",
  "Reed - dry" = "#8B7B00", "Reed - water" = "#4682B4",
  "Uncertain - dry" = "#CD853F", "Uncertain - water" = "#5F9EA0",
  "Inundated - dry" = "#FF0000", "Not inundated - water" = "#FFA500"
)

# ==============================================================================
# 1️⃣ Load Input Data (Raster)
# ==============================================================================
message("\n--- Loading input data for '", study_site_name, "' (", target_year, ") ---")

if (!file.exists(attributes_raster_path)) {
  stop("FATAL: Attributes raster not found at: \n", attributes_raster_path)
}
attributes_raster <- rast(attributes_raster_path)
message("Attributes raster loaded.")

if (!"mixture_category" %in% names(attributes_raster)) {
  stop("FATAL: 'mixture_category' layer not found in attributes_raster. Cannot filter for pure pixels.")
}
message("'mixture_category' layer found in attributes_raster.")

# ==============================================================================
# 2️⃣ Prepare Raster for WiW Classification
# ==============================================================================
message("\n--- Preparing raster for WiW classification ---")

# UPDATED: Define the specific bands required by the WiW model
required_bands <- c("b8a_scaled", "b12_scaled")

# Standardize layer names to lowercase for consistency
names(attributes_raster) <- tolower(names(attributes_raster))
message("Raster layer names standardized to lowercase.")

# Check if the required scaled bands exist
if (!all(required_bands %in% names(attributes_raster))) {
  stop("FATAL: Not all required scaled bands (b8a_scaled, b12_scaled) were found in the attributes raster.")
}

# Select only the layers needed for the WiW rule
predictor_stack <- subset(attributes_raster, required_bands)
message("Required predictor layers (NIR and SWIR2) selected.")

# ==============================================================================
# 3️⃣ Apply WiW Model and Save Classified Map
# ==============================================================================
message("\n--- Applying WiW model to generate and save classified map ---")

# UPDATED: Apply the WiW decision rules using raster algebra
# The rule: water if NIR <= 0.1804 AND SWIR2 <= 0.1131
# This creates a logical raster where 1 = water and 0 = dry
classified_map_logical <- predictor_stack$b8a_scaled <= 0.1804 & predictor_stack$b12_scaled <= 0.1131

# Convert the logical raster (0s and 1s) to a categorical raster with values 1 and 2
# We create a reclassification matrix: from -> to
# 0 (dry) -> 1
# 1 (water) -> 2
rcl_matrix <- matrix(c(0, 1, 1, 2), ncol = 2, byrow = TRUE)
classified_map <- terra::classify(classified_map_logical, rcl = rcl_matrix)
message("WiW classification complete.")

# --- Define and apply categories and a color map ---
levels(classified_map) <- data.frame(value = c(1, 2), class = c("dry", "water"))
color_map <- data.frame(value = c(1, 2), color = c("#dc5199", "#4cd2de")) # dry, water
coltab(classified_map) <- color_map
message("Color map and labels applied to the raster.")

# Save the final classified map
dir.create(dirname(classified_map_path), recursive = TRUE, showWarnings = FALSE)
writeRaster(classified_map, classified_map_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))
message("SUCCESS: Classified map saved to:\n", classified_map_path)


# ==============================================================================
# 4️⃣ Prepare Base Data for Visualization
# ==============================================================================
# This section is identical to your original script and works correctly.
message("\n--- Preparing base data for visualization ---")

reference_map <- attributes_raster[["dominant_label"]]
mixture_category_map <- attributes_raster[["mixture_category"]]

correct_ref_categories <- data.frame(
  value = c(1, 2, 3, 4, 5),
  category = c("Inundated", "Not inundated", "Other", "Reed", "Uncertain")
)
terra::set.cats(reference_map, value = correct_ref_categories)

correct_mix_categories <- data.frame(
  value = as.integer(c(1, 2, 3)),
  category = c("pure", "mixed", "very mixed")
)
terra::set.cats(mixture_category_map, value = correct_mix_categories)

ref_df <- terra::as.data.frame(reference_map, xy = TRUE, na.rm = FALSE)
names(ref_df)[3] <- "Reference"; ref_df$Reference <- as.character(ref_df$Reference)

class_df <- terra::as.data.frame(classified_map, xy = TRUE, na.rm = FALSE)
names(class_df)[3] <- "class_pixel_value"; class_df$class_pixel_value <- as.integer(class_df$class_pixel_value)
class_cats <- cats(classified_map)[[1]]
class_df <- class_df %>%
  left_join(class_cats, by = c("class_pixel_value" = "value")) %>%
  rename(Classified = class) %>%
  select(x, y, Classified); class_df$Classified <- as.character(class_df$Classified)

mixture_category_df <- terra::as.data.frame(mixture_category_map, xy = TRUE, na.rm = FALSE)
names(mixture_category_df)[3] <- "mixture_pixel_value"; mixture_category_df$mixture_pixel_value <- as.integer(mixture_category_df$mixture_pixel_value)
mixture_category_df <- mixture_category_df %>%
  left_join(correct_mix_categories, by = c("mixture_pixel_value" = "value")) %>%
  rename(MixtureCategory = category) %>%
  dplyr::select(x, y, MixtureCategory)

base_comparison_df <- full_join(ref_df, class_df, by = c("x", "y")) %>%
  full_join(mixture_category_df, by = c("x", "y")) %>%
  filter(!(is.na(Reference) & is.na(Classified)))
message("Base comparison data frame prepared.")

# ==============================================================================
# 5️⃣ Plotting Function Definition
# ==============================================================================
plot_comparison_maps <- function(data_to_plot, title_suffix, filename_suffix) {
  message(paste0("\n--- Generating plot: ", title_suffix, " ---"))
  
  current_plot_df_long <- data_to_plot %>%
    mutate(Combination = paste(Reference, "-", Classified)) %>%
    pivot_longer(
      cols = c("Reference", "Classified", "Combination"),
      names_to = "map_type", values_to = "label"
    ) %>%
    mutate(map_type = factor(map_type, levels = c("Reference", "Classified", "Combination")))
  
  all_possible_combinations <- unique(current_plot_df_long$label[current_plot_df_long$map_type == "Combination"])
  current_combination_colors <- combination_colors_base
  missing_combination_labels <- setdiff(all_possible_combinations, names(current_combination_colors))
  if (length(missing_combination_labels) > 0) {
    fallback_colors <- setNames(rep("transparent", length(missing_combination_labels)), missing_combination_labels)
    current_combination_colors <- c(current_combination_colors, fallback_colors)
  }
  
  comparison_plot <- ggplot(data = current_plot_df_long, aes(x = x, y = y)) +
    geom_raster(data = . %>% filter(map_type == "Reference"), aes(fill = label)) +
    scale_fill_manual(name = "Reference Label", values = reference_colors, na.value = "transparent", drop = FALSE) +
    new_scale_fill() +
    geom_raster(data = . %>% filter(map_type == "Classified"), aes(fill = label)) +
    # UPDATED: Legend title changed to "WiW Model"
    scale_fill_manual(name = "WiW Model", values = classified_colors, na.value = "transparent", drop = FALSE) +
    new_scale_fill() +
    geom_raster(data = . %>% filter(map_type == "Combination"), aes(fill = label)) +
    scale_fill_manual(name = "Agreement/Disagreement", values = current_combination_colors, na.value = "transparent", drop = FALSE) +
    facet_wrap(~ map_type) +
    labs(
      # UPDATED: Title changed to "WiW Model"
      title = paste0("Reference vs. WiW Model Classification (", title_suffix, ")"),
      subtitle = paste("Site:", study_site_name, "| Year:", target_year)
    ) +
    coord_equal() + theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      strip.text = element_text(face = "bold", size = 14),
      legend.position = "bottom", legend.box = "vertical",
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  print(comparison_plot)
  plot_filename <- file.path(plots_output_dir, paste0(study_site_name, "_", target_year, "_", filename_suffix, ".png"))
  ggsave(filename = plot_filename, plot = comparison_plot, width = 15, height = 7, dpi = 300, bg = "white")
  message("SUCCESS: Comparison map saved to:\n", plot_filename)
}

# ==============================================================================
# 6️⃣ Generate Plots
# ==============================================================================
# UPDATED: Filenames changed to refer to "wiw"
plot_comparison_maps(
  data_to_plot = base_comparison_df %>% dplyr::select(-MixtureCategory),
  title_suffix = "All Pixels",
  filename_suffix = "all_pixels_comparison_map_wiw_model"
)

pure_pixels_df <- base_comparison_df %>% filter(MixtureCategory == "pure")
if(nrow(pure_pixels_df) > 0) {
  plot_comparison_maps(
    data_to_plot = pure_pixels_df,
    title_suffix = "Pure Pixels Only",
    filename_suffix = "pure_pixels_comparison_map_wiw_model_pure"
  )
} else {
  message("\nSkipping plot for 'Pure Pixels Only' as no pure pixels were found.")
}

# ==============================================================================
# 7️⃣ Create and Save Separate Categorical Rasters
# ==============================================================================
message("\n--- Creating and saving separate categorical rasters for each map type ---")

# UPDATED: The full function definition with the custom sorting logic is now included
create_and_save_categorical_raster <- function(data_df, column_name, base_path, template_raster) {
  message(paste0("\nProcessing '", column_name, "' layer..."))
  
  unique_categories <- data_df %>%
    filter(!is.na(.data[[column_name]])) %>%
    distinct(category = .data[[column_name]])
  
  if (column_name == "Combination") {
    message("Applying custom sort order for 'Combination' layer.")
    desired_order <- names(combination_colors_base)
    unique_categories$category <- factor(unique_categories$category, levels = desired_order)
    mapping_df <- unique_categories %>%
      arrange(category) %>%
      mutate(value = 1:n(), category = as.character(category)) %>%
      select(value, category)
  } else {
    mapping_df <- unique_categories %>%
      arrange(category) %>%
      mutate(value = 1:n()) %>%
      select(value, category)
  }
  
  message("Created a mapping for ", nrow(mapping_df), " unique classes."); print(mapping_df)
  
  join_condition <- setNames("category", column_name)
  df_with_ids <- data_df %>% left_join(mapping_df, by = join_condition)
  raster_df <- df_with_ids %>% dplyr::select(x, y, value)
  
  categorical_raster <- terra::rast(raster_df, type = "xyz", crs = crs(template_raster))
  levels(categorical_raster) <- mapping_df
  
  raster_path <- paste0(base_path, "_", tolower(column_name), "_map.tif")
  legend_path <- paste0(base_path, "_", tolower(column_name), "_map_legend.csv")
  
  writeRaster(categorical_raster, raster_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW"), datatype = "INT1U"))
  message("SUCCESS: Categorical raster saved to:\n", raster_path)
  
  write.csv(mapping_df, legend_path, row.names = FALSE)
  message("SUCCESS: Raster legend saved to:\n", legend_path)
}

base_comparison_df <- base_comparison_df %>% mutate(Combination = paste(Reference, "-", Classified))

# UPDATED: Base path changed to refer to "wiw"
output_base_path <- file.path(
  output_root_dir, "classified_maps", study_site_name,
  paste0(study_site_name, "_", target_year, "_wiw")
)

create_and_save_categorical_raster(base_comparison_df, "Reference", output_base_path, attributes_raster)
create_and_save_categorical_raster(base_comparison_df, "Classified", output_base_path, attributes_raster)
create_and_save_categorical_raster(base_comparison_df, "Combination", output_base_path, attributes_raster)
