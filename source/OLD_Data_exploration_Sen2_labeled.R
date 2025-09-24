# 1. Load necessary packages
message("Loading required packages...")
suppressPackageStartupMessages({
  library(sf)       # For reading spatial data (though points_final_df is non-spatial)
  library(dplyr)    # For data manipulation
  library(units)    # Used in earlier steps, might not be directly needed in Step 6
  library(ggplot2)  # For plotting
  library(terra)    # Used in earlier steps
  library(tidyr)    # For data reshaping (pivot_longer)
  library(rpart)    # For decision trees
})
message("Packages loaded.")

# --- 2. Source Utility and Configuration Files ---
message("Sourcing utility and configuration files...")
# Define paths to the utility and configuration files
# Assumed to be in a 'source' subdirectory relative to this main script.
gdrive_utils_path <- "source/gdrive_utils.R" 
spatial_utils_path <- "source/spatial_processing_utils.R" 
config_path <- "source/config.R"          

# Helper function to check and source a file
source_if_exists <- function(file_path, file_description) {
  if (!file.exists(file_path)) {
    stop(paste(file_description, "not found at:", normalizePath(file_path, mustWork = FALSE),
               ". Please check the path and ensure the file exists relative to your working directory: ", getwd()))
  }
  message("Sourcing ", file_description, " from: ", normalizePath(file_path))
  # Source into the global environment (or the calling environment) so functions are directly available
  source(file_path, local = FALSE, chdir = TRUE) 
}

# Source utility and config files
source_if_exists(gdrive_utils_path, "Google Drive utilities (gdrive_utils.R)")
source_if_exists(config_path, "Configuration settings (config.R)") # Loads variables like study_site_name
source_if_exists(spatial_utils_path, "Spatial processing utilities (spatial_processing_utils.R)")


input_file_directory <- file.path(output_root_dir, "points", study_site_name, study_year)
input_filename <- paste0(study_site_name, "_", study_year, "_points_with_s2_values.gpkg") # Assuming this was the name of the file with ALL columns
input_gpkg_path <- file.path(input_file_directory, input_filename)
input_layer_name <- "points_with_s2_values" 

label_column_name <- "Label"

# 3. Read the GeoPackage file (which contains ALL original columns + ALL S2 bands)
message(paste("Loading full points data from:", input_gpkg_path, "Layer:", input_layer_name))
if (!file.exists(input_gpkg_path)) {
  stop(paste("Input file not found:", input_gpkg_path,
             "\nPlease ensure the path and filename are correct and point to the GeoPackage with ALL columns."))
}

tryCatch({
  points_data_full <- sf::st_read(input_gpkg_path, layer = input_layer_name)
  message(paste("Successfully loaded '", nrow(points_data_full), "' features with '", ncol(points_data_full) -1, "' attribute fields from layer '", input_layer_name, "'.", sep="")) # -1 for geometry
  message("Columns in loaded data (points_data_full):")
  print(names(points_data_full))
  # print(head(points_data_full)) # Uncomment to see first few rows
}, error = function(e) {
  stop(paste("Error loading GeoPackage:", e$message))
})


# 4. Subset columns: Keep geometry, original 'Label' column, columns starting with "B", and "SCL"
message("\nSubsetting columns to keep original '",label_column_name,"', columns starting with 'B', and 'SCL' (plus geometry)...", sep="")

all_loaded_names <- names(points_data_full)
geometry_col_name <- attr(points_data_full, "sf_column")

# Check if the defined label_column_name exists
if (!label_column_name %in% all_loaded_names) {
  warning(paste("WARNING: The specified 'label_column_name' ('", label_column_name, "') was NOT FOUND in the loaded data. ",
                "The 'Label' attribute will be missing from the subset. Available columns: ",
                paste(all_loaded_names, collapse=", "), sep=""))
  # To prevent errors later, set it to NULL if not found, so it's not included in selection
  label_col_to_actually_keep <- NULL
} else {
  label_col_to_actually_keep <- label_column_name
}

# Identify columns starting with "B"
s2_b_band_names <- all_loaded_names[startsWith(all_loaded_names, "B")]

# Identify the "SCL" column
scl_column_name_exact <- "SCL"
actual_scl_col_to_keep <- NULL
if (scl_column_name_exact %in% all_loaded_names) {
  actual_scl_col_to_keep <- scl_column_name_exact
  message(paste("INFO: Found '", scl_column_name_exact, "' column.", sep=""))
} else {
  scl_candidates <- all_loaded_names[grepl("SCL", all_loaded_names, ignore.case = TRUE)]
  if(length(scl_candidates) == 1){
    actual_scl_col_to_keep <- scl_candidates[1]
    message(paste("INFO: Exact 'SCL' not found, but found SCL-like column:", actual_scl_col_to_keep))
  } else if (length(scl_candidates) > 1) {
    actual_scl_col_to_keep <- scl_candidates[1]
    message(paste("WARNING: Exact 'SCL' not found. Multiple SCL-like columns found:", paste(scl_candidates, collapse=", "), ". Using the first one:", actual_scl_col_to_keep, ". Please verify."))
  } else {
    message(paste("WARNING: Column named '", scl_column_name_exact, "' or similar not found. SCL data will not be included in the subset.", sep=""))
  }
}

# Combine attribute columns to select
attributes_to_select <- character(0)
if (!is.null(label_col_to_actually_keep)) { # Add the label column if it was found
  attributes_to_select <- c(attributes_to_select, label_col_to_actually_keep)
}
if (length(s2_b_band_names) > 0) {
  attributes_to_select <- c(attributes_to_select, s2_b_band_names)
}
if (!is.null(actual_scl_col_to_keep)) {
  attributes_to_select <- c(attributes_to_select, actual_scl_col_to_keep)
}
attributes_to_select <- unique(attributes_to_select)
attributes_to_select <- attributes_to_select[attributes_to_select %in% all_loaded_names]


# Final list of columns for selection (attributes + geometry column)
final_columns_for_selection <- attributes_to_select
if (!is.null(geometry_col_name) && geometry_col_name %in% all_loaded_names) {
  final_columns_for_selection <- c(final_columns_for_selection, geometry_col_name)
} else {
  warning("Geometry column problem during selection setup.")
}
final_columns_for_selection <- unique(final_columns_for_selection)

# Perform the subsetting
points_data_subset <- NULL
if (length(final_columns_for_selection) > 0 && (!is.null(geometry_col_name) && geometry_col_name %in% final_columns_for_selection) ) {
  message(paste("Subsetting to keep columns:", paste(final_columns_for_selection, collapse=", ")))
  points_data_subset <- points_data_full[, final_columns_for_selection, drop = FALSE]
  message("Columns subsetted successfully.")
  message("Columns in the subsetted data ('points_data_subset'):")
  print(names(points_data_subset))
} else {
  message("WARNING: No columns matched the specified criteria or geometry column was invalid. Keeping all loaded columns.")
  points_data_subset <- points_data_full
}


# =============================================================
# Step 5: Calculate Spectral Indices, Standardize Columns, and Drop Geometry
# (This step now operates on 'points_data_subset' which includes "Label")
# =============================================================
message("\nStarting Step 5: Calculate Indices, Standardize Columns, Drop Geometry...")
if (!exists("points_data_subset") || !inherits(points_data_subset, "sf")) {
  stop("Object 'points_data_subset' not found or is not an sf object. Please ensure Step 4 ran correctly.")
}
current_points_data <- points_data_subset

# 1. Convert all existing column names to lowercase
message(" -> Converting all column names to lowercase...")
original_geom_col_name <- attr(current_points_data, "sf_column")
names(current_points_data) <- tolower(names(current_points_data))
new_geom_col_name <- tolower(original_geom_col_name)
if (new_geom_col_name %in% names(current_points_data)) {
  sf::st_geometry(current_points_data) <- new_geom_col_name
} else if (!is.null(original_geom_col_name) && original_geom_col_name %in% names(current_points_data) && original_geom_col_name != new_geom_col_name) {
  sf::st_geometry(current_points_data) <- original_geom_col_name
  message(paste("    Retained original geometry column name for sf object:", original_geom_col_name))
} else {
  warning(paste("    Could not definitively reset geometry column name after lowercasing. Expected:", new_geom_col_name, "or", original_geom_col_name))
}
# Now, the original label column (e.g., "Label") will be "label" if label_column_name was "Label"
message(paste("    Column names after lowercasing:", paste(names(current_points_data), collapse=", ")))

# 2. Define the custom function for STR index
swir_to_str <- function(swir_band_values) {
  swir_scaled <- swir_band_values / 10000
  str_values <- ifelse(swir_scaled <= 0, NA_real_, ((1 - swir_scaled)^2) / (2 * swir_scaled))
  return(str_values)
}
message(" -> Custom function 'swir_to_str' defined.")

# 3. Calculate Indices
if (nrow(current_points_data) == 0) {
  message("WARN: 'current_points_data' is empty. No indices to calculate.")
  points_with_indices <- current_points_data %>%
    dplyr::mutate(
      ndvi = numeric(0), ndwi_mf = numeric(0), mndwi11 = numeric(0),
      mndwi12 = numeric(0), ndmi_gao11 = numeric(0),
      str1 = numeric(0), str2 = numeric(0)
    )
} else {
  required_bands_lower <- c("b03", "b04", "b8a", "b11", "b12")
  missing_bands <- required_bands_lower[!(required_bands_lower %in% names(current_points_data))]
  if (length(missing_bands) > 0) {
    stop(paste("Error: After lowercasing, required band columns missing:", paste(missing_bands, collapse=", "),
               "\nAvailable:", paste(names(current_points_data), collapse=", ")))
  }
  message(" -> Calculating spectral indices...")
  points_with_indices <- current_points_data %>%
    dplyr::mutate(
      ndvi = (.data$b8a - .data$b04) / (.data$b8a + .data$b04),
      ndwi_mf = (.data$b03 - .data$b8a) / (.data$b03 + .data$b8a),
      mndwi11 = (.data$b03 - .data$b11) / (.data$b03 + .data$b11),
      mndwi12 = (.data$b03 - .data$b12) / (.data$b03 + .data$b12),
      ndmi_gao11 = (.data$b8a - .data$b11) / (.data$b8a + .data$b11),
      str1 = swir_to_str(.data$b11),
      str2 = swir_to_str(.data$b12)
    )
  message("    Spectral indices calculated.")
}

# 4. Drop the 'scl' column (now lowercase 'scl')
scl_col_to_drop_lower <- "scl"
if (scl_col_to_drop_lower %in% names(points_with_indices)) {
  message(paste(" -> Dropping column:", scl_col_to_drop_lower))
  points_with_indices[[scl_col_to_drop_lower]] <- NULL
} else {
  message(paste(" -> Column '", scl_col_to_drop_lower, "' not found to drop.", sep=""))
}

# 5. Drop the geometry column
if (inherits(points_with_indices, "sf")) {
  message(" -> Dropping geometry column...")
  geom_col_name_to_drop <- attr(points_with_indices, "sf_column")
  if (is.null(geom_col_name_to_drop) && new_geom_col_name %in% names(points_with_indices)) geom_col_name_to_drop <- new_geom_col_name
  
  points_final_df <- sf::st_drop_geometry(points_with_indices)
  message(paste("    Geometry column '", geom_col_name_to_drop %||% "(active geometry)", "' dropped. Result is now a data frame.", sep=""))
} else {
  message(" -> Object is already not an sf object.")
  points_final_df <- points_with_indices
}

# Display info about the final data frame
message("\n--- Output of Step 5: points_final_df (now a data frame/tibble) ---")
message(paste("Number of rows:", nrow(points_final_df)))
message("Final column names (all lowercase, 'scl' and geometry dropped, 'label' kept):")
print(names(points_final_df))

# The original label column (e.g. "Label") should now be present as "label" (if label_column_name was "Label")
# along with lowercased B-bands and lowercased new indices.
# Verify if 'label' (or tolower(label_column_name)) is in names(points_final_df)
expected_label_col_lower <- tolower(label_column_name)
if (expected_label_col_lower %in% names(points_final_df)) {
  message(paste("INFO: Original label column '", label_column_name, "' successfully kept as '", expected_label_col_lower, "'.", sep=""))
} else {
  message(paste("WARNING: Original label column '", label_column_name, "' (expected as '",expected_label_col_lower,"') is NOT in the final data frame. Check Step 2 & 4.", sep=""))
}

message("Sample of data (showing some indices and the label):")
cols_to_show_sample <- intersect(
  c(tolower(label_column_name), "b03", "b04", "b8a", "b11", "b12", "ndvi", "ndwi_mf", "mndwi11", "mndwi12", "ndmi_gao11", "str1", "str2"),
  names(points_final_df)
)
if (length(cols_to_show_sample) > 0 && nrow(points_final_df) > 0) {
  print(head(points_final_df[, cols_to_show_sample]))
} else if (nrow(points_final_df) > 0) {
  print(head(points_final_df))
} else {
  message("No data to show in sample as points_final_df is empty.")
}

label_col_for_counting <- tolower(label_column_name) # Use the lowercased version

if (label_col_for_counting %in% names(points_final_df)) {
  message(paste("\nNumber of points per class in '", label_col_for_counting, "' column:", sep=""))
  # Using dplyr::count for a tidy table output
  class_counts <- points_final_df %>%
    dplyr::count(.data[[label_col_for_counting]], name = "count_of_points") # Use .data[[]] for robustness
  
  print(class_counts) # This will print a tibble with columns: 'label' (or actual name) and 'count_of_points'
  
  # Alternative using base R's table(), which gives a named vector or table object:
  # message("\nNumber of points per class (using base R table()):")
  # print(table(points_final_df[[label_col_for_counting]], dnn = paste("Class in '", label_col_for_counting, "'", sep="")))
} else {
  message(paste("\nWARNING: Label column '", label_col_for_counting,
                "' (expected from '", label_column_name, "') not found in points_final_df. Cannot count points per class.", sep=""))
  message(paste("   Available columns are:", paste(names(points_final_df), collapse = ", ")))
}

message("\nStep 5: Spectral index calculation, column standardization, and geometry drop finished.")


message("\nStarting Step 6: Generating Faceted Boxplots by Label with Custom Colors...")

# Ensure 'points_final_df' exists and is a data frame
if (!exists("points_final_df") || !is.data.frame(points_final_df)) {
  stop("Object 'points_final_df' not found or is not a data frame. Please ensure Step 5 ran correctly.")
}

if (nrow(points_final_df) == 0) {
  message("Data frame 'points_final_df' is empty. Cannot generate boxplots.")
  # End of Step 6 if no data
} else {
  
  # The label column for grouping should be 'label' (as it was lowercased in Step 5)
  label_col_for_grouping <- "label"
  
  if (!label_col_for_grouping %in% names(points_final_df)) {
    # Attempt to find the original label column if 'label' is not present
    if (exists("label_column_name") && tolower(label_column_name) %in% names(points_final_df)) {
      label_col_for_grouping <- tolower(label_column_name)
      message(paste("INFO: Using '", label_col_for_grouping, "' as the grouping column for boxplots.", sep=""))
    } else if (exists("label_column_name") && label_column_name %in% names(points_final_df)) {
      label_col_for_grouping <- label_column_name
      message(paste("INFO: Using '", label_col_for_grouping, "' (original case) as the grouping column for boxplots.", sep=""))
    } else {
      stop(paste("The grouping column '", label_col_for_grouping, "' was not found in 'points_final_df'. ",
                 "Available columns: ", paste(names(points_final_df), collapse=", "), sep=""))
    }
  }
  
  # Convert the label column to factor to ensure correct ordering and for custom color mapping
  # Important: The levels of this factor must match the names in your custom color vector.
  points_final_df[[label_col_for_grouping]] <- as.factor(points_final_df[[label_col_for_grouping]])
  message(paste("Levels for '", label_col_for_grouping, "': ", paste(levels(points_final_df[[label_col_for_grouping]]), collapse=", "), sep=""))
  
  
  # Identify numeric columns for plotting
  cols_for_plotting <- names(points_final_df)[sapply(points_final_df, is.numeric)]
  
  if (length(cols_for_plotting) == 0) {
    message("No numeric columns found in 'points_final_df' to generate boxplots for.")
  } else {
    message(paste("Numeric columns identified for boxplots:", paste(cols_for_plotting, collapse=", ")))
    
    # Reshape data to long format for faceting
    points_long_format <- points_final_df %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols_for_plotting),
        names_to = "metric",
        values_to = "value"
      )
    message("Data reshaped to long format for plotting.")
    
    # Define your custom colors
    # Names MUST match the levels in points_final_df[[label_col_for_grouping]]
    # Since column names (and thus 'label') were lowercased in Step 5, use lowercase keys here.
    custom_label_colors <- c(
      "inundated" = "#4cd2de",
      "not inundated" = "#dc5199",
      "other" = "#86eb79",
      "uncertain" = "#ff7f00"
    )
    message("Custom colors defined. Ensure these names match the unique values in your 'label' column.")
    # Check if all label levels have a color defined
    current_labels_in_data <- levels(points_final_df[[label_col_for_grouping]])
    if(!all(current_labels_in_data %in% names(custom_label_colors))) {
      warning(paste("Not all label levels in data have a custom color defined! Missing colors for:",
                    paste(current_labels_in_data[!current_labels_in_data %in% names(custom_label_colors)], collapse=", "),
                    ". Default ggplot2 colors will be used for these."))
    }
    
    
    # Create the faceted boxplot
    tryCatch({
      boxplot_collection <- ggplot2::ggplot(
        points_long_format,
        ggplot2::aes(x = .data[[label_col_for_grouping]], y = value, fill = .data[[label_col_for_grouping]])
      ) +
        ggplot2::geom_boxplot(na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = custom_label_colors) + # Apply custom colors
        ggplot2::facet_wrap(~ metric, scales = "free_y") +
        ggplot2::labs(
          title = "Distribution of Sentinel-2 Bands and Indices by Label Class",
          subtitle = if(exists("study_site_name") && exists("study_year")) {
            paste("Site:", study_site_name, "| Year:", study_year)
          } else { "" },
          x = "Label Class",
          y = "Value (units vary by metric)",
          fill = "Label Class"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          strip.text = ggplot2::element_text(size = 7),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          legend.position = "top"
        )
      
      # Print the plot to the R plots pane
      print(boxplot_collection)
      message("Faceted boxplots generated. Check the R plots pane.")
      
      # Save the plot collection to a file
      if (exists("output_root_dir") && exists("study_site_name") && exists("study_year")) {
        plot_output_dir <- file.path(output_root_dir, "boxplots", study_site_name, study_year)
        if (!dir.exists(plot_output_dir)) {
          dir.create(plot_output_dir, recursive = TRUE, showWarnings = TRUE)
        }
        # Create a filename for the single multi-panel plot
        plot_filename <- paste0(study_site_name, "_", study_year, "_boxplots_all_metrics.png")
        plot_filepath <- file.path(plot_output_dir, plot_filename)
        
        tryCatch({
          # Adjust width/height based on the number of facets for better readability
          num_metrics <- length(unique(points_long_format$metric))
          # Heuristic for plot dimensions:
          cols_facet <- floor(sqrt(num_metrics))
          if (cols_facet == 0) cols_facet <- 1 # Avoid division by zero if num_metrics is 0 or 1
          rows_facet <- ceiling(num_metrics / cols_facet)
          
          plot_width <- max(10, cols_facet * 3.5) # Adjust base width and multiplier as needed
          plot_height <- max(8, rows_facet * 3)  # Adjust base height and multiplier as needed
          
          ggplot2::ggsave(filename = plot_filepath, plot = boxplot_collection,
                          width = plot_width, height = plot_height, dpi = 300, units = "in", limitsize = FALSE)
          message(paste("Faceted boxplot saved to:", plot_filepath))
        }, error = function(e_save) {
          warning(paste("Could not save the boxplot image:", e_save$message))
        })
      } else {
        message("WARNING: 'output_root_dir', 'study_site_name', or 'study_year' not defined. Plot displayed but not saved to structured directory.")
      }
      
    }, error = function(e_plot) {
      warning(paste("An error occurred while generating the boxplots:", e_plot$message))
    })
  } # End of if (length(cols_for_plotting) > 0)
} # End of if (nrow(points_final_df) > 0)

message("\nStep 6: Faceted boxplot generation and saving finished.")

message("\nStarting Step 7: Applying Saved Decision Tree Model...")

# --- User Input: Path to Model File ---
model_rdata_path <- "source/jussila_decisiontree.RData" # As specified by user

# --- Load the .RData file containing the model ---
model_env <- new.env() # Create a new environment to load the .RData into
if (!file.exists(model_rdata_path)) {
  stop(paste("Model file not found at:", model_rdata_path))
}
message(paste("Loading decision tree model from:", model_rdata_path))
# 'load()' loads objects into the specified environment. It returns the names of loaded objects.
loaded_object_names_from_rdata <- load(model_rdata_path, envir = model_env)

if (length(loaded_object_names_from_rdata) == 0) {
  stop("No objects were loaded from the .RData file. Please check the file path and content.")
}
message(paste("Objects loaded from .RData file:", paste(loaded_object_names_from_rdata, collapse=", ")))

# --- Assign the specific model object ('tree_jussila') ---
model_object_name_in_file <- "tree_jussila" # User specified this name
loaded_decision_tree <- NULL # Initialize

if (model_object_name_in_file %in% loaded_object_names_from_rdata) {
  loaded_decision_tree <- model_env[[model_object_name_in_file]]
  message(paste("Successfully assigned model object '", model_object_name_in_file, "' to 'loaded_decision_tree'.", sep=""))
  message(paste("Class of loaded_decision_tree:", paste(class(loaded_decision_tree), collapse=", ")))
} else {
  stop(paste("Model object named '", model_object_name_in_file, "' was NOT FOUND in the loaded .RData file. ",
             "Available loaded objects are: ", paste(loaded_object_names_from_rdata, collapse=", "), sep=""))
}

if (is.null(loaded_decision_tree)) {
  # This should ideally be caught by the stop above, but as a final safeguard.
  stop("Failed to assign the loaded decision tree model object. 'loaded_decision_tree' is NULL.")
}

# Example check for rpart, you might need to change 'rpart' if your model is from a different package
# This helps confirm you have the right package loaded for the predict method.
if (inherits(loaded_decision_tree, "rpart")) {
  message("Loaded model appears to be an 'rpart' tree model.")
  # Ensure rpart is loaded if not done in Step 1 explicitly for some reason
  if (!"package:rpart" %in% search()) {
    warning("Model is 'rpart' but 'rpart' package doesn't seem loaded. Prediction might fail. Please load 'rpart' via library(rpart).")
  }
} else {
  message(paste("INFO: Loaded model is of class '", paste(class(loaded_decision_tree), collapse=", "),
                "'. Ensure the corresponding package is loaded for predict() to work correctly.", sep=""))
}


# --- Identify Predictor Variables for the Model ---
# Based on your comment "first five indices needed for applying Jussila-model"
# These were calculated in Step 5 and their names were lowercased.
predictor_cols <- c("ndvi", "ndwi_mf", "mndwi11", "mndwi12", "ndmi_gao11")

# Ensure these predictor columns exist in points_final_df (which is output of Step 5)
if (!exists("points_final_df") || !is.data.frame(points_final_df)) {
  stop("Data frame 'points_final_df' not found. Ensure Step 5 completed successfully.")
}
missing_predictors <- predictor_cols[!(predictor_cols %in% names(points_final_df))]
if (length(missing_predictors) > 0) {
  stop(paste("Error: The following predictor columns required by the model are missing from 'points_final_df':",
             paste(missing_predictors, collapse=", "),
             "\nAvailable columns in 'points_final_df' are:", paste(names(points_final_df), collapse=", ")))
}
message(paste("Using predictor columns for the model:", paste(predictor_cols, collapse=", ")))

# Select only the predictor columns for the 'newdata' argument in predict()
data_for_prediction <- points_final_df[, predictor_cols, drop = FALSE]

# --- Make Predictions ---
message("Making predictions using the loaded decision tree ('tree_jussila')...")
model_predictions <- NULL # Initialize
tryCatch({
  # The 'type' argument in predict() depends on the model package and desired output.
  # For 'rpart' classification trees, type="class" gives predicted class labels.
  if (inherits(loaded_decision_tree, "rpart")) {
    model_predictions <- predict(loaded_decision_tree, newdata = data_for_prediction, type = "class")
  } else {
    # For other model types (e.g., randomForest, ranger, C5.0, tree)
    # the default predict might give class labels, or probabilities, or a specific 'type' argument is needed.
    # Defaulting to a generic predict call. User might need to adjust 'type' based on their model.
    message("Attempting generic predict(). If model is not 'rpart', ensure 'type' argument is appropriate or default gives class labels.")
    model_predictions <- predict(loaded_decision_tree, newdata = data_for_prediction)
    # If prediction output is a matrix (often probabilities for each class), get the class with the max probability.
    if(is.matrix(model_predictions) && ncol(model_predictions) > 1 && !is.null(colnames(model_predictions))) {
      message("Prediction output is a matrix (possibly probabilities). Assigning class with the highest probability.")
      model_predictions <- colnames(model_predictions)[apply(model_predictions, 1, which.max)]
    } else if (is.list(model_predictions) && !is.data.frame(model_predictions)) {
      # Some predict methods return lists, e.g. $class
      if (!is.null(model_predictions$class)) {
        model_predictions <- model_predictions$class
        message("Extracted class predictions from list output (model_predictions$class).")
      } else if (!is.null(model_predictions$predicted)) {
        model_predictions <- model_predictions$predicted
        message("Extracted class predictions from list output (model_predictions$predicted).")
      } else {
        warning("Prediction output is a list, but could not automatically extract class labels. Manual inspection needed.")
      }
    }
  }
  message("Predictions made successfully.")
  
  # Add predictions as a new column to points_final_df
  points_final_df$predicted_jussila <- model_predictions
  
  message("Predictions added as a new column 'predicted_jussila'.")
  message("Sample of data with original label (if kept) and predicted label:")
  
  original_label_col_lower <- tolower(label_column_name) # from Step 2, now lowercased
  
  cols_to_show_comparison <- c(original_label_col_lower, "predicted_jussila")
  cols_to_show_comparison <- cols_to_show_comparison[cols_to_show_comparison %in% names(points_final_df)]
  
  if(length(cols_to_show_comparison) > 0 && nrow(points_final_df) > 0){
    print(head(points_final_df[, cols_to_show_comparison]))
  } else if (nrow(points_final_df) > 0) {
    print(head(points_final_df[, "predicted_jussila", drop=FALSE])) # Show at least predictions
  }
  
  # Optional: Create a confusion matrix if the original 'label' column is present and considered ground truth
  if (original_label_col_lower %in% names(points_final_df)) {
    message(paste("\nComparison of original '",original_label_col_lower,"' with 'predicted_jussila':", sep=""))
    try({
      actual_labels_factor <- as.factor(points_final_df[[original_label_col_lower]])
      # Ensure predicted_jussila is also a factor with potentially the same levels for a good table
      # If model_predictions was already factor, this is fine. If character, convert.
      predicted_labels_factor <- as.factor(points_final_df$predicted_jussila)
      
      # For a simple table, levels don't strictly need to match, but for caret::confusionMatrix they should.
      confusion_table <- table(Actual = actual_labels_factor, Predicted = predicted_labels_factor)
      print(confusion_table)
      
      # If you have 'caret' package installed, for more stats:
      # if (requireNamespace("caret", quietly = TRUE)) {
      #   # Ensure levels match for caret's confusionMatrix for full stats
      #   common_levels <- intersect(levels(actual_labels_factor), levels(predicted_labels_factor))
      #   if (length(common_levels) >= 2) { # Need at least 2 common levels for meaningful matrix
      #       actual_for_caret <- factor(actual_labels_factor, levels = common_levels)
      #       predicted_for_caret <- factor(predicted_labels_factor, levels = common_levels)
      #       print(caret::confusionMatrix(data = predicted_for_caret, reference = actual_for_caret))
      #   } else {
      #       message("Not enough common levels between actual and predicted labels for detailed confusion matrix via caret.")
      #   }
      # }
    }, silent = TRUE)
  }
  
}, error = function(e) {
  message("--- PREDICTION ERROR ---")
  message("Error during model prediction: ", e$message)
  message(
    "Please ensure:\n",
    "1. The correct R package for your 'tree_jussila' model (e.g., 'rpart', 'randomForest') is installed AND loaded in Step 1.\n",
    "2. 'loaded_decision_tree' correctly refers to the 'tree_jussila' model object.\n",
    "3. The predictor columns in 'points_final_df' (", paste(predictor_cols, collapse=", "), ") exactly match (names, data types, and ideally order) those used when 'tree_jussila' was trained.\n",
    "4. If your model is not 'rpart', the 'predict()' call might need a specific 'type' argument (e.g., type='response' for randomForest class predictions)."
  )
  message("--- END PREDICTION ERROR ---")
})

message("\nStep 7: Decision tree application finished.")




