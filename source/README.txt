--- DESCRIPTION SCRIPTS---

- Script: Crosstab_Cal_Area_BWK.R - 
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
  
  
  


- Post-processing LabelMe output.R - 
In this script, the shapefile with individual(and possibly overlapping) 
polygons is converted to a geopackage with non-overlapping polygons 
that completely fill the selected tiles.

The input shapefile was created with 'Convert_json_to_polygons_new.py' 
in the folder 2025_LabelMe/Labelme.

It works with 3 classes (in order of priority in case of overlap):
- inundated 
- other
- uncertain
- not inundated

The latter (not inundated) is not created in LabelMe, but is the background 
class that fills up the non-classified areas. 

This script has two helper scripts
- config.R - contains the input parameters that the user needs to change before
running the script
- grdive-utils.R - contains the functions needed for downloading the 
necessary data to the local 'data' folder
- spatial_processing_utils.R - contains the functions needed for the actual
  spatial processing
  
