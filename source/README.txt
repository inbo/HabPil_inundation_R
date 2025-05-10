--- DESCRIPTION SCRIPTS---



- Post-processing LabelMe output.R - 
In this script, the shapefile with individual(and possibly overlapping) 
polygons is converted to a geopackage with non-overlapping polygons 
that completely fill the selected tiles.
.
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