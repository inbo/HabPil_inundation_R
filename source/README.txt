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
	This R script automates the acquisition of Sentinel-2 L2A satellite imagery for a specific date, study 	site, and year. It leverages the openEO platform to connect to the Copernicus Data Space Ecosystem, 	process data on the cloud, and deliver analysis-ready imagery.

	Key Features & Analysis Performed:
	- Parameterized Acquisition: The script is configured to run for different study sites and years by 	adjusting variables in the setup section.
	- Dynamic Date Lookup: It automatically retrieves the exact Sentinel-2 acquisition date from a centralized 	CSV lookup table stored on Google Drive, ensuring consistency across analyses.
	- Automated Boundary Ingestion: Downloads the appropriate study site boundary (Area of Interest) shapefile 	from a designated Google Drive folder.
	- Cloud-Based Processing: Connects to the openEO backend, authenticates the user, and defines a data cube 	to load the Sentinel-2 L2A collection for the specified spatial and temporal extent.
	- Asynchronous Job Management: Creates, starts, and monitors an openEO batch job to process the data 	remotely without requiring local computational resources for the initial data filtering.
	- Result Handling: Downloads the resulting GeoTIFF file from the completed openEO job to a local 	directory.
	- Automated Output Management: Renames the downloaded file to a standardized format (e.g., StudySite_Sen2	_Year.tif) and automatically uploads it to a specified results folder on Google Drive.


ThisR project has two helper scripts
- config.R - contains the input parameters that the user needs to change before
running the script
- grdive-utils.R - contains the functions needed for downloading the 
necessary data to the local 'data' folder
- spatial_processing_utils.R - contains the functions needed for the actual
  spatial processing
  
