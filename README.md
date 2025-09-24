<!-- badges: start -->
![GitHub](https://img.shields.io/github/license/inbo/HabPil_inundation_R)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/inbo/HabPil_inundation_R/check-project)
![GitHub repo size](https://img.shields.io/github/repo-size/inbo/HabPil_inundation_R)
<!-- badges: end -->

# Inundation Habitats Pilot

[Heremans, Stien![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-5356-1093)[^aut][^cre][^inbo.be]
Research Institute for Nature and Forest (INBO)[^cph][^fnd]

[^cph]: copyright holder
[^fnd]: funder
[^aut]: author
[^cre]: contact person
[^inbo.be]: Instituut voor Natuur- en Bosonderzoek (INBO)


**keywords**: remote sensing; inundation; satellites; wetlands; grasslands

<!-- community: inbo -->

<!-- description: start -->
This R project provides a complete workflow for acquiring Sentinel-2 satellite imagery, processing it against ground-truth data, and validating an inundation detection model. The analysis places a special emphasis on understanding how model performance is affected by the purity of land cover within each pixel.

---
## Key Features 📜
* **Automated Data Pipeline**: Scripts are designed to run sequentially, creating a reproducible analysis pipeline from raw data to final validation.
* **Cloud-Based Image Acquisition**: Leverages **openEO** to process Sentinel-2 data on the Copernicus Data Space Ecosystem, minimizing local processing requirements.
* **Pixel Purity Analysis**: A core feature is the analysis of how **mixed pixels** affect model accuracy, providing deeper insights than a simple accuracy score.
* **Centralized Configuration**: Easily run the entire workflow for different study sites or years by modifying a single `config.R` file.
* **Integrated Data Management**: Seamlessly downloads source data from and uploads results to **Google Drive**, ensuring data is organized and accessible.
* **Rich Visualization**: Generates a suite of outputs, including boxplots of spectral signatures, performance metric charts, and detailed three-panel comparison maps for spatial validation.

---
## Workflow Overview

The project follows a sequential workflow, where the output of one script often serves as the input for the next.

1.  **Data Acquisition (`OpenEO_get_Sen2_data_inundation.R`)**: Downloads Sentinel-2 L2A imagery for a specific site and date using the openEO platform.
2.  **Ground-Truth Preparation (`Create_fractional_coverage_raster.R`)**: Converts labeled vector polygons (ground-truth) into a fractional cover raster that is perfectly aligned with the Sentinel-2 grid.
3.  **Data Extraction (`Extract_labeled_pixel_values.R`)**: Extracts the spectral values from the Sentinel-2 image for pixels identified in the ground-truth raster, creating a tidy dataset for analysis.
4.  **Tabular Analysis & Validation (`Analyze_and_Model_Pixel_Data.R`)**: Uses the extracted data to perform a detailed numerical evaluation of a pre-trained decision tree model, calculating performance metrics (Recall, Precision, F1-Score) stratified by pixel purity.
5.  **Spatial Analysis & Mapping (`Apply_jussila_model_map.R`)**: Applies the decision tree model to the entire study area to produce a classified inundation map. It generates comparison plots to visually assess the spatial accuracy of the model against the ground truth.

---
## Getting Started

### Prerequisites

* R (version 4.2 or higher)
* RStudio IDE (recommended)
* Access to Google Drive and the necessary permissions for the project folders.
* An openEO platform account for satellite data acquisition.

### Installation

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/inbo/HabPil_inundation_R.git](https://github.com/inbo/HabPil_inundation_R.git)
    cd HabPil_inundation_R
    ```
2.  **Install R Packages:** Open the R project and run the following command in the console to install all required packages.
    ```R
    install.packages(c("terra", "dplyr", "ggplot2", "rpart", "googledrive", "openeo", "tidyr", "scales", "ggnewscale"))
    ```
3.  **Configure Environment:**
    * Authenticate Google Drive by running `googledrive::drive_auth()` in the console and following the prompts.
    * Modify the `config.R` file to set your desired `study_site_name`, `target_year`, and relevant Google Drive folder IDs.

---
## Scripts Overview 🔎

This project is organized into a series of primary analysis scripts and helper scripts that contain reusable functions.

### Main Scripts

#### `OpenEO_get_Sen2_data_inundation.R`
* **Purpose**: Automates the acquisition of Sentinel-2 L2A satellite imagery for a specific date, study site, and year using the openEO platform.

#### `Create_fractional_coverage_raster.R`
* **Purpose**: Generates foundational fractional cover rasters from labeled polygons. These rasters are essential for understanding mixed pixel compositions.

#### `Extract_labeled_pixel_values.R`
* **Purpose**: Creates a structured dataset for machine learning by extracting spectral values from Sentinel-2 imagery corresponding to the ground-truth labels.

#### `Analyze_and_Model_Pixel_Data.R`
* **Purpose**: Performs the final numerical analysis and validation of a pre-trained inundation model using the extracted pixel data.

#### `Apply_jussila_model_map.R`
* **Purpose**: Transitions from tabular analysis to spatial validation by applying the model to the entire study area raster.

#### `Crosstab_Cal_Area_BWK.R`
* **Purpose**: An accessory script to analyze the Biological Valuation Map (BWK) vector data, focusing on the area and distribution of habitats within grassland and wetland units.

### Helper Scripts

* `config.R`: Contains the input parameters (e.g., `study_site_name`, `target_year`) that the user needs to change before running the workflow.
* `gdrive-utils.R`: Contains functions for downloading data from and uploading results to Google Drive.
* `spatial_processing_utils.R`: Contains functions for core spatial processing tasks used by the main scripts.

<!-- description: end -->
