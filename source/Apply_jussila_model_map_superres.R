# ==============================================================================
# Apply Jussila Model and Create Comparison Visualization
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
# study_site_name <- "Webbekomsbroek"
# study_site_name <- "Schulensmeer"
# study_site_name <- "Kloosterbeemden"
 study_site_name <- "Webbekomsbroek2"

# target_year <- 2020
# target_year <- 2021
# target_year <- 2023
 target_year <- 2024
 
# Add a resolution identifier for managing filenames
spatial_resolution <- "superres"

# --- Root Directory ---
output_root_dir <- "output"

# --- Input File Paths ---
# Path to the superres raster
attributes_raster_path <- file.path(
  output_root_dir, "final_rasters", study_site_name, target_year,
  paste0(study_site_name, "_", target_year, "_final_attributes_raster_", spatial_resolution, ".tif")
)

# Path to the pre-trained decision tree model
model_path <- "source/jussila_decisiontree.RData"

# --- Output File Paths ---
# Including year subfolder for plots
plots_output_dir <- file.path(output_root_dir, "plots", study_site_name, target_year)
dir.create(plots_output_dir, recursive = TRUE, showWarnings = FALSE)


classified_map_path <- file.path(
  output_root_dir, "classified_maps", study_site_name,
  paste0(study_site_name, "_", target_year, "_jussila_classified_map_", spatial_resolution, ".tif")
)

# --- Plotting Parameters (identical to original) ---
reference_colors <- c(
  "Inundated" = "#4cd2de", "Not inundated" = "#dc5199", "Other" = "#86eb79",
  "Uncertain" = "#ff7f00", "Reed" = "#c49c02"
)
classified_colors <- c("water" = "#4cd2de", "dry" = "#dc5199")
combination_colors_base <- c(
  "Inundated - water" = "#008080", "Not inundated - dry" = "#8B4513",
  "Other - dry" = "#6B8E23", "Other - water" = "#1E90FF",
  "Reed - dry" = "#8B7B00", "Reed - water" = "#4682B4",
  "Uncertain - dry" = "#CD853F", "Uncertain - water" = "#5F9EA0",
  "Inundated - dry" = "#FF0000", "Not inundated - water" = "#FFA500"
)

# ==============================================================================
# 1️⃣ Load Input Data (Raster and Model)
# ==============================================================================
message("\n--- Loading input data for '", study_site_name, "' (", target_year, ") ---")
if (!file.exists(attributes_raster_path)) stop("FATAL: Attributes raster not found.")
attributes_raster <- rast(attributes_raster_path)
message("Attributes raster loaded.")
if (!"mixture_category" %in% names(attributes_raster)) stop("FATAL: 'mixture_category' layer not found.")
if (!file.exists(model_path)) stop("FATAL: Model file not found.")
load(model_path, envir = .GlobalEnv)
message("Decision tree model 'tree_jussila' loaded successfully.")

# ==============================================================================
# 2️⃣ Prepare Raster for Prediction
# ==============================================================================
message("\n--- Preparing raster by selecting and renaming predictor layers ---")
model_predictor_names <- attr(tree_jussila$terms, "term.labels")
message("Model requires predictors: ", paste(model_predictor_names, collapse = ", "))
names(attributes_raster) <- tolower(names(attributes_raster))
message("Raster layer names standardized to lowercase.")
if (!all(model_predictor_names %in% names(attributes_raster))) stop("FATAL: Not all required predictor layers were found.")
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

color_map <- data.frame(value = c(1, 2), color = c("#dc5199", "#4cd2de"))
coltab(classified_map) <- color_map
message("Color map applied to the raster.")
writeRaster(classified_map, classified_map_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))
message("SUCCESS: Classified map saved to:\n", classified_map_path)

# ==============================================================================
# 4️⃣ Prepare Base Data for Visualization
# ==============================================================================
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
names(ref_df)[3] <- "Reference"
ref_df$Reference <- as.character(ref_df$Reference)

class_df <- terra::as.data.frame(classified_map, xy = TRUE, na.rm = FALSE)
names(class_df)[3] <- "class_pixel_value"
class_df$class_pixel_value <- as.integer(class_df$class_pixel_value)
class_cats <- cats(classified_map)[[1]]
if (is.null(class_cats) || nrow(class_cats) == 0) {
  class_df <- class_df %>% mutate(Classified = as.character(class_pixel_value)) %>% select(x, y, Classified)
} else {
  class_df <- class_df %>%
    left_join(class_cats, by = c("class_pixel_value" = "value")) %>%
    rename(Classified = class) %>%
    select(x, y, Classified)
}
class_df$Classified <- as.character(class_df$Classified)

mixture_category_df <- terra::as.data.frame(mixture_category_map, xy = TRUE, na.rm = FALSE)
names(mixture_category_df)[3] <- "mixture_pixel_value"
mixture_category_df$mixture_pixel_value <- as.integer(mixture_category_df$mixture_pixel_value)
correct_mix_categories_join <- data.frame(
  value = as.integer(c(1, 2, 3)),
  category = c("pure", "mixed", "very mixed")
)
mixture_category_df <- mixture_category_df %>%
  left_join(correct_mix_categories_join, by = c("mixture_pixel_value" = "value")) %>%
  rename(MixtureCategory = category) %>%
  dplyr::select(x, y, MixtureCategory)

base_comparison_df <- full_join(ref_df, class_df, by = c("x", "y")) %>%
  full_join(mixture_category_df, by = c("x", "y")) %>%
  filter(!(is.na(Reference) & is.na(Classified)))
message("Base comparison data frame prepared, including MixtureCategory values.")

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
  
  all_combos <- unique(current_plot_df_long$label[current_plot_df_long$map_type == "Combination"])
  current_colors <- combination_colors_base
  missing_labels <- setdiff(all_combos, names(current_colors))
  if (length(missing_labels) > 0) {
    fallback <- setNames(rep("transparent", length(missing_labels)), missing_labels)
    current_colors <- c(current_colors, fallback)
  }
  
  comparison_plot <- ggplot(data = current_plot_df_long, aes(x = x, y = y)) +
    geom_raster(data = . %>% filter(map_type == "Reference"), aes(fill = label)) +
    scale_fill_manual(name = "Reference Label", values = reference_colors, na.value = "transparent", drop = FALSE) +
    new_scale_fill() +
    geom_raster(data = . %>% filter(map_type == "Classified"), aes(fill = label)) +
    scale_fill_manual(name = "Jussila Model", values = classified_colors, na.value = "transparent", drop = FALSE) +
    new_scale_fill() +
    geom_raster(data = . %>% filter(map_type == "Combination"), aes(fill = label)) +
    scale_fill_manual(name = "Agreement/Disagreement", values = current_colors, na.value = "transparent", drop = FALSE) +
    facet_wrap(~ map_type) +
    labs(
      title = paste0("Reference vs. Jussila Model Classification (", title_suffix, ")"),
      subtitle = paste("Site:", study_site_name, "| Year:", target_year, "| Resolution:", spatial_resolution)
    ) +
    coord_equal() + theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5), strip.text = element_text(face = "bold"),
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
plot_comparison_maps(
  data_to_plot = base_comparison_df %>% dplyr::select(-MixtureCategory),
  title_suffix = "All Pixels",
  filename_suffix = paste0("all_pixels_comparison_map_jussila_model_", spatial_resolution)
)

pure_pixels_df <- base_comparison_df %>% filter(MixtureCategory == "pure")
if(nrow(pure_pixels_df) > 0) {
  plot_comparison_maps(
    data_to_plot = pure_pixels_df %>% dplyr::select(-MixtureCategory),
    title_suffix = "Pure Pixels Only",
    filename_suffix = paste0("pure_pixels_comparison_map_jussila_model_pure_", spatial_resolution)
  )
} else {
  message("\nSkipping plot for 'Pure Pixels Only' as no pure pixels were found.")
}

# ==============================================================================
# 7️⃣ Create and Save Separate Categorical Rasters
# ==============================================================================
message("\n--- Creating and saving separate categorical rasters for each map type ---")
create_and_save_categorical_raster <- function(data_df, column_name, base_path, template_raster) {
  message(paste0("\nProcessing '", column_name, "' layer..."))
  mapping_df <- data_df %>%
    filter(!is.na(.data[[column_name]])) %>% distinct(category = .data[[column_name]]) %>%
    arrange(category) %>% mutate(value = 1:n()) %>% dplyr::select(value, category)
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


output_base_path <- file.path(
  output_root_dir, "classified_maps", study_site_name,
  paste0(study_site_name, "_", target_year, "_jussila_", spatial_resolution)
)

create_and_save_categorical_raster(base_comparison_df, "Reference", output_base_path, attributes_raster)
create_and_save_categorical_raster(base_comparison_df, "Classified", output_base_path, attributes_raster)
create_and_save_categorical_raster(base_comparison_df, "Combination", output_base_path, attributes_raster)

