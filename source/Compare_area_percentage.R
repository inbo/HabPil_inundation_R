# ==============================================================================
# Compare Predicted vs. Reference Water Area Percentages (with Shapes)
# ==============================================================================

# Load Required Libraries
# ==============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)
library(scales)

# ==============================================================================
# 0️⃣ Configuration and Setup
# ==============================================================================

# --- Directory Paths ---
metrics_root_dir <- "output/pixel_data_tables"
plot_output_dir <- "output/summary_plots"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# --- Plotting Parameters ---
plot_colors <- c(
  "Original" = "#0072B2", # Blue
  "SuperRes" = "#D55E00"  # Orange/Red
)

# ==============================================================================
# 1️⃣ Aggregate All Area Percentage Data
# ==============================================================================
message("--- Searching for and aggregating all water area percentage files ---")

area_perc_files <- list.files(
  path = metrics_root_dir,
  pattern = "_water_area_percentages\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(area_perc_files) == 0) {
  stop("FATAL: No '_water_area_percentages.csv' files found in '", metrics_root_dir, "'.")
}

all_area_perc_df <- map_dfr(area_perc_files, read.csv)

message("Successfully loaded and combined ", length(area_perc_files), " area percentage files.")

# ==============================================================================
# 2️⃣ Prepare Data for Plotting
# ==============================================================================
message("\n--- Preparing aggregated data for scatter plot ---")

plot_data <- all_area_perc_df %>%
  mutate(site_year = paste(study_site, year)) %>%
  pivot_longer(
    cols = c(Predicted_Orig_Water_Percent, Predicted_SuperRes_Water_Percent),
    names_to = "Resolution_Raw",
    values_to = "Predicted_Water_Percent"
  ) %>%
  mutate(
    Resolution = if_else(grepl("Orig", Resolution_Raw), "Original", "SuperRes")
  ) %>%
  select(study_site, site_year, Reference_Water_Percent, Predicted_Water_Percent, Resolution)

message("Data reshaped for plotting:")
print(head(plot_data))

# ==============================================================================
# 3️⃣ Generate and Save the Scatter Plot
# ==============================================================================
message("\n--- Generating area percentage comparison scatter plot ---")

# --- NEW: Calculate Nash-Sutcliffe Efficiency (NSE) ---
# This is the "R-squared for the 1-1 line"
# NSE = 1 - (Sum of Squared Errors from 1-1 line) / (Sum of Squared Deviations from Reference Mean)
agreement_data <- plot_data %>%
  group_by(Resolution) %>%
  summarize(
    # Sum of Squared Errors relative to the 1-1 line
    sse_1_to_1 = sum((Predicted_Water_Percent - Reference_Water_Percent)^2),
    # Sum of Squared Total (variance of reference data)
    sst_ref_mean = sum((Reference_Water_Percent - mean(Reference_Water_Percent))^2),
    .groups = 'drop'
  ) %>%
  # Calculate NSE
  mutate(
    NSE = 1 - (sse_1_to_1 / sst_ref_mean),
    # Create a label for the plot
    nse_label = sprintf("R² = %.3f", NSE),
    # Define X and Y positions for the labels (top-left corner)
    Reference_Water_Percent = 5, # X position
    Predicted_Water_Percent = ifelse(Resolution == "Original", 98, 92) # Y positions
  )

message("Agreement metrics (NSE) calculated:")
print(agreement_data)


# --- Create the plot ---
area_comparison_plot <- ggplot(plot_data, aes(x = Reference_Water_Percent, y = Predicted_Water_Percent)) +
  # Add the 1:1 line first
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  
  # Add the points, mapping color to Resolution and shape to study_site
  geom_point(aes(color = Resolution, shape = study_site), size = 3.5, alpha = 0.8, stroke = 1.2) +
  
  # --- REMOVED: Regression lines are not relevant to NSE ---
  # geom_smooth(aes(color = Resolution), method = "lm", se = FALSE, formula = y ~ x, linetype = "solid") +
  
  # --- NEW: Add NSE text labels ---
  geom_text(
    data = agreement_data,
    aes(label = nse_label, color = Resolution),
    show.legend = FALSE, # Hide legend for the text
    size = 4,
    fontface = "bold",
    hjust = 0 # Left-align the text
  ) +
  
  # Apply custom colors
  scale_color_manual(name = "Resolution", values = plot_colors) +
  
  # Define shapes
  scale_shape_manual(name = "Study Site", values = c(16, 17, 15, 18, 8, 9)) +
  
  # Set axis limits and labels to percentages
  scale_x_continuous(limits = c(0, 100), name = "Reference Water %", labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(limits = c(0, 100), name = "Predicted Water %", labels = scales::percent_format(scale = 1)) +
  
  # Add titles
  labs(
    title = "Predicted vs. Reference Water percentage",
    subtitle = "Comparison across all study sites and years"
  ) +
  
  # Apply theme
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.box = "vertical",
    aspect.ratio = 1
  )

print(area_comparison_plot)

# --- Save the plot to a file ---
output_filename <- file.path(plot_output_dir, "water_area_percentage_comparison_bysite.png")
ggsave(
  output_filename,
  plot = area_comparison_plot,
  width = 9.5,
  height = 8.5,
  dpi = 300
)

message("\nSUCCESS: Area percentage comparison plot saved to:\n", output_filename)

