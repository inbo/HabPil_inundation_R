library(inbodb)
library(DBI)
library(ggplot2)

con <- connect_inbo_dbase("W0002_10_Watina")

timeseries <- dbGetQuery(con, "SELECT 
	 DatumWID		= CONVERT(INT,pm.DatumWID)
		, PeilpuntCode	= CONVERT(VARCHAR(10),pp.PeilpuntCode)
		, Datum			= CONVERT(DATETIME, CONVERT(VARCHAR(8), pm.DatumWID), 112)
		, mTAW			= CONVERT(FLOAT, pm.mTAW)
		, mMaaiveld		= CONVERT(FLOAT, pm.mMaaiveld)
		, Operator		= CONVERT(VARCHAR(500),mw.MedewerkerVoornaam + ' ' + mw.MedewerkerNaam)
		, Remark		= CONVERT(VARCHAR(100),'')
		, Gevalideerd	= CONVERT(VARCHAR(100),pm.PeilmetingStatus)
		, Methode		= CONVERT(VARCHAR(100),mt.MetingTypeNaam)
		, pm.PeilmetingCategorieCode
		, pm.PeilmetingCommentaar
		, pp.PeilpuntVersie
	FROM dbo.FactPeilMeting pm
		LEFT JOIN dbo.DimPeilpunt pp ON pp.PeilpuntWID = pm.PeilpuntWID 
		LEFT JOIN dbo.DimMeetpunt mp ON mp.MeetpuntWID = pp.MeetpuntWID 
		LEFT JOIN dbo.DimMedewerker mw ON mw.MedewerkerWID = pm.MedewerkerWID
		LEFT JOIN (SELECT DISTINCT PeilMetingTypeCode AS MetingTypeCode
							, CASE WHEN PeilMetingTypeCode = 'BUIT' THEN 'Meting buiten de buis'
							   WHEN PeilMetingTypeCode = 'CLBR' THEN 'Kalibratiemeting'
							   WHEN PeilMetingTypeCode = 'DIVER' THEN 'Sondemeting'
							   WHEN PeilMetingTypeCode = 'HAND' THEN 'Handmatige meting'
								END AS MetingTypeNaam
					FROM dbo.FactPeilMeting 
					UNION ALL
					SELECT 'ONBEK', 'Onbekend'
					) AS mt ON mt.MetingTypeCode = pm.PeilMetingTypeCode
where mp.GebiedWID = '389'
order by datumwid
")

p1 <- ggplot(timeseries, aes(x=Datum, y=mMaaiveld, colour=PeilpuntCode)) +
  geom_line()

p1

dbDisconnect(con)
rm(con)
