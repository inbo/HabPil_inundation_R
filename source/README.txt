--- DESCRIPTION SCRIPTS---

Crosstab_Cal_Area_BWK.R 
	Purpose:
	This R script automates the analysis of Biological Valuation Map (BWK) data for specified
	study sites. It focuses on characterizing the spatial distribution and
	area contributions of Natura2000 habitats and Regionally Important Biotopes (RBBs)
	within BWK-defined grassland and wetland units.

	Key Features & Analysis Performed:
	- Parameterized Analysis: Designed to run for any specified study site by
  	 	configuring its corresponding Google Drive folder ID.
	- Data Acquisition: Automatically downloads the necessary BWK shapefile
 	 	components from Google Drive to a local cache.
	- BWK Unit Classification: Extracts and categorizes BWK units into
  		'grassland' (EENH1 starting with 'h') and 'wetland' (EENH1 starting with 'm').
	- Habitat Cross-Tabulation (Heatmaps): Generates a cross-tabulation showing
  		the area distribution of HAB1 classes within EENH1 units, visualized as heatmaps.
	- Habitat Area Calculation (Bar Plots): Calculates the total area (in hectares)
  		for each HAB1 class (including 'gh' for non-habitat areas),
  		separately for grassland and wetland units.
	- Ordered Bar Plot Visualization: Produces ordered bar plots that visually
  		represent the calculated areas of HAB1 classes within grassland and wetland units,
  		sorted by area.
	- Spatial Data Export: Creates separate GeoPackage (.gpkg) files for the filtered
  		grassland and wetland BWK data.
	- Automated Output Management: All generated plots (.png) and GeoPackage files (.gpkg)
  		are automatically saved to a local output directory and uploaded to predefined
  		Google Drive folders.

OpenEO_get_Sen2_data_inundation.R
	Purpose:
	This R script automates the acquisition of Sentinel-2 L2A satellite imagery for a specific date, study 	site, and year. It leverages the openEO platform to connect to the Copernicus Data Space Ecosystem, 	process data on the 	cloud, and deliver analysis-ready imagery.

	Key Features & Analysis Performed:
	- Parameterized Acquisition: The script is configured to run for different study sites and years by 	adjusting variables in the setup section.
	- Dynamic Date Lookup: It automatically retrieves the exact Sentinel-2 acquisition date from a centralized 	CSV lookup table stored on Google Drive, ensuring consistency across analyses.
	- Automated Boundary Ingestion: Downloads the appropriate study site boundary (Area of Interest) shapefile 	from a designated Google Drive folder.
	- Cloud-Based Processing: Connects to the openEO backend, authenticates the user, and defines a data cube 	to load the Sentinel-2 L2A collection for the specified spatial and temporal extent.
	- Asynchronous Job Management: Creates, starts, and monitors an openEO batch job to process the data 	remotely without requiring local computational resources for the initial data filtering.
	- Result Handling: Downloads the resulting GeoTIFF file from the completed openEO job to a local 	directory.
	- Automated Output Management: Renames the downloaded file to a standardized format (e.g., StudySite_Sen2	_Year.tif) and automatically uploads it to a specified results folder on Google Drive.
	
	
Create fractional coverage raster.R
  	Purpose:
  	This script generates the foundational fractional cover rasters, which are essential inputs for subsequent analyses involving       spectral mixture analysis, pixel purity assessment, and the generation of training data for 	machine learning models that interpret   mixed pixel compositions.
  
  	Key Features & Analysis Performed:
  	- Input Data Acquisition: Downloads clean, labeled polygon shapefiles from Google Drive, caching them locally. It handles site-specific naming conventions for input shapefiles.
  	- Template Grid Integration: Loads a Sentinel-2 image as a template to define the spatial grid (resolution, extent, Coordinate Reference System) for the output fractional raster, ensuring perfect alignment with satellite 	imagery.
  	- Fractional Cover Calculation: Utilizes the create_fraction_raster utility function to rasterize the input polygons. For each output raster pixel, it calculates the proportion (fractional cover) of each unique land cover label 	("Label" field in the input shapefile) that falls within that pixel. This results in a multi-band raster where each band corresponds to a distinct label's fractional presence.
  	- Spatial Alignment: Automatically reprojects input polygons if their CRS does not match the Sentinel-2 template raster, ensuring seamless integration.
	- Output Management: Saves the resulting fractional cover raster as a GeoTIFF file, organized into a structured output directory (output/fraction_rasters/[study_site_name]/[target_year]).


Extract_labeled_pixel_values.R
  	Purpose:
  	This script is designed to create a structured dataset for machine learning applications by extracting spectral values from Sentinel-2 satellite imagery for pixels that have been previously labeled with ground-truth data. It 	effectively links remote sensing data with land cover classes, producing a clean, analysis-ready table of spectral signatures.

  	Key Features & Analysis Performed:
  	- Data Ingestion: Automatically downloads the necessary Sentinel-2 imagery and the corresponding fractional cover rasters from designated Google Drive folders.
  	- Pixel Purity Filtering: Identifies and selects "pure" pixels by applying a fractional cover threshold. This ensures that the extracted spectral data accurately represents a single, specific land cover class (e.g., only pixels 	with >90% cover of a given class are used).
  	- Spectral Data Extraction: For each pure pixel identified, the script extracts the corresponding spectral values from all bands of the Sentinel-2 raster.
  	- Data Aggregation: The extracted pixel values are compiled into a tidy data frame, where each row represents a pure pixel and includes its spectral signatures along with its corresponding land cover label.
  	- Automated Output Management: The final dataset is saved as a CSV file to a local output directory and is also uploaded to a specified Google Drive folder, making it readily available for training and validating classification 	models.


Analyze_and_Model_Pixel_Data.R
  	Purpose:
  	This script serves as the final analysis and validation step in the workflow. It takes the processed pixel-level data (containing spectral indices and ground-truth labels) and evaluates the performance of a pre-trained 	inundation detection model (Jussila decision tree). The core objective is to quantify the model's accuracy and understand how its performance is influenced by pixel purity (i.e., how mixed the land cover is within a single 	pixel).

  	Key Features & Analysis Performed:
  	- Data Ingestion: Loads the pre-processed CSV file containing spectral indices, fractional cover information, and dominant land cover labels for each pixel.

  	Exploratory Data Visualization:
  	- Boxplots: Generates boxplots for "pure" pixels (>90% single class coverage) to visualize and compare the spectral signatures of different land cover classes across various indices (NDVI, NDWI, etc.).
  	- Stacked Bar Plots: Creates a comprehensive stacked bar plot showing the total count of pixels for each dominant label, segmented by purity class ("pure", "mixed", "very_mixed"). This provides a clear overview of the dataset's 	composition.

  	Model Application:
  	- Loads a pre-trained decision tree model (.RData file).
  	- Applies the model to the pixel data to predict a class (e.g., 'water' or 'dry') for every pixel based on its spectral characteristics.

  	Quantitative Performance Evaluation:
  	- Confusion Matrix: Generates a confusion matrix comparing the ground-truth dominant labels against the model's predictions.
 	- Stratified Metrics Calculation: Calculates key performance metrics (Recall, Precision, and F1-Score) for the 'water' and 'dry' classes. Crucially, these metrics are calculated separately for each pixel purity level ("pure", 	"mixed", and "very_mixed").
  	- Performance Trend Visualization: Produces a line plot that clearly illustrates how the model's Recall, Precision, and F1-Score change as pixel mixture increases. This is the key output for assessing model robustness.

  	- Automated Output Management: All generated plots (boxplots, bar plots, performance metrics) are automatically saved as PNG files into a structured output directory, organized by study site and year.
  
  
Apply_jussila_model_map.R

  	Purpose:
  	This script transitions from tabular analysis to spatial validation. Its primary goal is to apply the pre-trained Jussila decision tree model to the entire study area's attribute raster, generating a full-coverage classified 	inundation map. It then produces a set of detailed, three-panel comparison plots to visually assess the spatial agreement and disagreement between the ground-truth reference labels and the model's predictions.

  	Key Features & Analysis Performed:
  	- Spatial Prediction: Loads the final multi-band attributes raster and the pre-trained decision tree model. It then performs a wall-to-wall classification using terra::predict to create a new raster map where each pixel is 	classified as either 'water' or 'dry'.
  	- Raster Data Integration: Extracts the necessary layers—the ground-truth dominant_label, the model's classified prediction, and the mixture_category—and converts them from raster format into a unified data frame based on pixel 	coordinates. This prepares the data for advanced visualization with ggplot2.
  	- Three-Panel Comparison Plots: Generates a powerful visual diagnostic plot containing three maps side-by-side:
    	Reference Map: Displays the ground-truth labels.
    	Classified Map: Shows the output from the Jussila model.
    	Agreement/Disagreement Map: A detailed view that uses a unique color for each combination of reference vs. predicted class, immediately highlighting correct classifications, false positives (Not inundated classified as water), 	and false negatives (Inundated classified as dry).

  	- Purity-Based Visual Filtering: The script generates the three-panel plot twice: once using all pixels in the study area, and a second time using only "pure" pixels (>90% single class coverage). This provides a direct visual 	comparison to assess how model performance and spatial accuracy improve when the complexity of mixed pixels is removed.

  	- Automated Output Management: Saves the final classified inundation map as a compressed GeoTIFF (.tif) and exports the high-resolution comparison plots as PNG files to the project's output directory.


ThisR project has two helper scripts

grdive-utils.R - contains the functions needed for downloading the necessary data to the local 'data' folder

spatial_processing_utils.R - contains the functions needed for the actual spatial processing
  
