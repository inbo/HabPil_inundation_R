library(inbodb)
library(DBI)
library(ggplot2)

con <- connect_inbo_dbase("W0002_10_Watina")

gebieden<- dbGetQuery(con, "SELECT *
	FROM dbo.DimGebied geb")

# Load necessary library
library(sf)
library(dplyr)  # For data manipulation

# Select only the required columns
gebieden_subset <- gebieden %>%
  select(GebiedWID, GebiedID, GebiedCode, GebiedNaam, GebiedXCoordinaat, GebiedYCoordinaat)

# Convert to an sf object
gebieden_sf <- st_as_sf(gebieden_subset, coords = c("GebiedXCoordinaat", "GebiedYCoordinaat"), crs =  31370) # (Belgian Lambert 72: EPSG 31370)

# Save as a GeoJSON file
st_write(gebieden_sf, "output/gebieden.geojson", driver = "GeoJSON")


meetpunten <- dbGetQuery(con, "SELECT *
	FROM dbo.DimMeetpunt mp")

# Select only the required columns
meetpunten_subset <- meetpunten %>%
  select(MeetpuntWID, MeetpuntID, MeetpuntCode, GebiedWID, MeetpuntXCoordinaat, MeetpuntYCoordinaat)

# Convert to an sf object
meetpunten_sf <- st_as_sf(meetpunten_subset, coords = c("MeetpuntXCoordinaat", "MeetpuntYCoordinaat"), crs =  31370) # (Belgian Lambert 72: EPSG 31370)

# Save as a GeoJSON file
st_write(meetpunten_sf, "output/meetpunten.geojson", driver = "GeoJSON")

# Read the shapefile of the Demervallei
demer_sf <- st_read("data/Demervallei_SBZ_afbakening.shp")  # Replace with actual file path

# Ensure both layers have the same CRS (Belgian Lambert 72: EPSG 31370)
demer_sf <- st_transform(demer_sf, crs = 31370)
meetpunten_sf <- st_transform(meetpunten_sf, crs = 31370)

# Perform spatial intersection: Keep only meetpunten inside the area
meetpunten_demer <- st_intersection(meetpunten_sf, demer_sf)

# Save the filtered meetpunten as a new GeoJSON file
st_write(meetpunten_demer, "output/meetpunten_Demervallei.geojson", driver = "GeoJSON")

# Extract unique MeetpuntWID values from meetpunten_demer
meetpunt_wids <- unique(meetpunten_demer$MeetpuntWID)

# Convert meetpunt_wids to a format suitable for SQL IN clause
meetpunt_wids_sql <- paste(meetpunt_wids, collapse = ",")

query <- sprintf("
  SELECT 
    pm.DatumWID,
    mp.MeetpuntID,      
    mp.MeetpuntCode,    
    pp.PeilpuntCode,    
    CONVERT(DATETIME, CONVERT(VARCHAR(8), pm.DatumWID), 112) AS Datum,
    pm.mTAW,  
    pm.mMaaiveld,  
    mw.MedewerkerVoornaam + ' ' + mw.MedewerkerNaam AS Operator,
    '' AS Remark,
    pm.PeilmetingStatus AS Gevalideerd,
    mt.MetingTypeNaam AS Methode,
    pm.PeilmetingCategorieCode,
    pm.PeilmetingCommentaar,
    pp.PeilpuntVersie
  FROM dbo.FactPeilMeting pm
  LEFT JOIN dbo.DimPeilpunt pp ON pp.PeilpuntWID = pm.PeilpuntWID 
  LEFT JOIN dbo.DimMeetpunt mp ON mp.MeetpuntWID = pp.MeetpuntWID  
  LEFT JOIN dbo.DimMedewerker mw ON mw.MedewerkerWID = pm.MedewerkerWID
  LEFT JOIN (SELECT DISTINCT PeilMetingTypeCode AS MetingTypeCode,
                     CASE WHEN PeilMetingTypeCode = 'BUIT' THEN 'Meting buiten de buis'
                          WHEN PeilMetingTypeCode = 'CLBR' THEN 'Kalibratiemeting'
                          WHEN PeilMetingTypeCode = 'DIVER' THEN 'Sondemeting'
                          WHEN PeilMetingTypeCode = 'HAND' THEN 'Handmatige meting'
                     END AS MetingTypeNaam
            FROM dbo.FactPeilMeting 
            UNION ALL
            SELECT 'ONBEK', 'Onbekend'
  ) AS mt ON mt.MetingTypeCode = pm.PeilMetingTypeCode
  WHERE pm.DatumWID >= 20180101  
    AND mp.MeetpuntWID IN (%s)
  ORDER BY pm.DatumWID
", meetpunt_wids_sql)


# Execute query and handle errors
peilmetingen_demer <- tryCatch({
  dbGetQuery(con, query)
}, error = function(e) {
  message("SQL query failed: ", e)
  NULL
})

# View results if query execution was successful
if (!is.null(peilmetingen_demer)) {
  View(peilmetingen_demer)
} else {
  message("No results retrieved.")
}
# Close database connection
dbDisconnect(con)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a directory to save the plots if it doesn't exist
output_dir <- "output/plots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Generate and save plots for each unique MeetpuntCode
for (meetpunt in unique(peilmetingen_demer$MeetpuntCode)) {
  # Filter data for the current MeetpuntCode
  meetpunt_data <- peilmetingen_demer %>% filter(MeetpuntCode == meetpunt)
  
  # Create the plot
  p <- ggplot(meetpunt_data, aes(x = Datum, y = mMaaiveld, colour = PeilpuntCode)) +
    geom_line() +
    labs(title = paste("Time Series Plot for", meetpunt),
         x = "Date",
         y = "mMaaiveld",
         color = "PeilpuntCode") +
    theme_minimal(base_size = 14) +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA))
  
  # Save the plot
  ggsave(filename = paste0(output_dir, "/", meetpunt, ".png"), plot = p, width = 8, height = 5, dpi = 300)
}

# Filter meetpunten that have measurements after 1 Jan 2018
meetpunten_filtered <- meetpunten_demer %>%
  filter(MeetpuntCode %in% unique(peilmetingen_demer$MeetpuntCode))

# Save as a new GeoJSON file
st_write(meetpunten_filtered, "output/meetpunten_Demervallei_actief2018.geojson", driver = "GeoJSON", delete_dsn = TRUE)
