# ==============================================================================
# Aggregate Boxplots: Bands vs Purity (Filtered, Reordered & Renamed)
# ------------------------------------------------------------------------------
# Purpose:
# Generates a grid plot (Rows=Bands, Cols=Purity).
# Filters for specific labels: Not inundated, Uncertain, Inundated.
# Renames variables to "Band X" format.
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
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
synthesis_plots_dir <- file.path(output_root_dir, "synthesis_plots")
dir.create(synthesis_plots_dir, showWarnings = FALSE, recursive = TRUE)

# --- Plotting Parameters ---
main_label_colors <- c(
  "Inundated" = "#4cd2de",
  "Not inundated" = "#dc5199",
  "Uncertain" = "#ff7f00"
)

target_labels <- c("Not inundated", "Uncertain", "Inundated")

# --- Define Variables to Plot ---
cols_to_plot <- c(
  "b04_scaled",    # Band 4
  "b8a_scaled",    # Band 8a (NIR)
  "b11_scaled",    # Band 11
  "b12_scaled",    # Band 12
  "mndwi12"        # MNDWI using B12
)

# ==============================================================================
# 1️⃣ Find and Load All Pixel Attribute Datasets
# ==============================================================================
message("\n--- Finding and loading all ORIGINAL resolution pixel attribute CSVs ---")

all_runs <- expand.grid(study_site = study_sites, year = years)
pixel_files <- file.path(
  output_root_dir, "pixel_data_tables", all_runs$study_site, all_runs$year,
  paste0(all_runs$study_site, "_", all_runs$year, "_final_pixel_attributes.csv")
)
existing_pixel_files <- pixel_files[file.exists(pixel_files)]

if (length(existing_pixel_files) == 0) stop("FATAL: No 'final_pixel_attributes.csv' files found.")

aggregated_pixels_df <- map_dfr(existing_pixel_files, function(file) {
  read.csv(file) %>% rename_with(tolower)
})
message("Successfully loaded data for ", format(nrow(aggregated_pixels_df), big.mark = ","), " pixels.")

# ==============================================================================
# 2️⃣ Prepare Data for Plotting
# ==============================================================================
message("\n--- Preparing data for grid plot ---")

# Filter, Select, and Reshape
plot_data_long <- aggregated_pixels_df %>%
  filter(dominant_label %in% target_labels) %>%
  select(dominant_label, mixture_category, all_of(cols_to_plot)) %>%
  pivot_longer(
    cols = -c(dominant_label, mixture_category),
    names_to = "variable_name",
    values_to = "value"
  ) %>%
  filter(is.finite(value))

# --- Clean up Factors and Labels ---

# 1. Dominant Label Order
plot_data_long$dominant_label <- factor(
  plot_data_long$dominant_label, 
  levels = target_labels
)

# 2. Mixture Category Cleaning
plot_data_long <- plot_data_long %>%
  mutate(
    mixture_category = stringr::str_replace(mixture_category, "_", " "),
    mixture_category = stringr::str_to_title(mixture_category),
    mixture_category = factor(mixture_category, levels = c("Pure", "Mixed", "Very Mixed"))
  )

# 3. RENAME VARIABLES (Bands)
# This replaces the raw column names with clean "Band X" labels
plot_data_long <- plot_data_long %>%
  mutate(
    variable_name = case_when(
      variable_name == "b04_scaled" ~ "Band 4",
      variable_name == "b8a_scaled" ~ "Band 8a",
      variable_name == "b11_scaled" ~ "Band 11",
      variable_name == "b12_scaled" ~ "Band 12",
      variable_name == "mndwi12"    ~ "MNDWI12",
      TRUE ~ variable_name # Fallback
    )
  )

# ==============================================================================
# 3️⃣ Generate and Save Grid Plot
# ==============================================================================
message("\n--- Generating grid boxplot ---")

p <- ggplot(plot_data_long, aes(x = dominant_label, y = value, fill = dominant_label)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  
  # --- FACET GRID ---
  # Rows = Variable (Renamed Bands), Columns = Purity Level
  facet_grid(rows = vars(variable_name), cols = vars(mixture_category), scales = "free_y") +
  
  scale_fill_manual(values = main_label_colors, name = "Dominant Label") +
  labs(
    title = "Spectral Signatures by Purity Level",
    x = "Reference Label",
    y = "Reflectance / Index Value"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines")
  )

print(p)

# --- Save the plot ---
output_filename <- file.path(synthesis_plots_dir, "ALL_SITES_aggregated_purity_grid_boxplot_renamed.png")

ggsave(
  output_filename,
  plot = p,
  width = 12, 
  height = 14,
  dpi = 300,
  limitsize = FALSE
)
message("Renamed grid boxplot saved to:\n", output_filename)

