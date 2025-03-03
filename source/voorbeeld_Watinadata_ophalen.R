#connectie naar Watina-databank (SQL-server): werkt buiten kantoor enkel met VPN-verbinding

#!opgelet om de peilmetingen op te halen, heb je toegang nodig tot de productiedatabank van Watina.

library(watina)
con <- connect_watina()

#extra functies ophalen
source(file.path("src", "VoerVoorMenyanthes.R"))

 
gebiedscode <- "ASH" #Achter Schoonhoven als test
#meetpunten selecteren
# alle meetpunten binnen de regio BOG:
#   Maximale diepte:10m, inclusief meetpunten waarvan de diepte van de filter onbekend is.
#   Type meetpunten: piëzometers/peilbuizen ('P'), peilschalen ('S') en oppervlaktewaterstalen ('R')
# debugonce(get_locs)
data_locs_ASH <- get_locs(con, filterdepth_range = c(0,10), filterdepth_guess = TRUE, filterdepth_na = TRUE, obswells = FALSE, obswell_aggr = "latest", area_codes = gebiedscode, loc_type = c("P", "R", "S"), collect = TRUE)

#bewaren locaties
saveRDS(data_locs, file.path("data","temp", "watina_meetpunten_bog_alle.rds"))
# data_locs_feb22 <- readRDS(file.path(path_to_interim_vdb, "watina_meetpunten_vdb_alle.rds"))
# all.equal(data_locs, data_locs_feb22)
#ok

dbDisconnect(con)

# alle beschikbare peildata
# Grondwater- en oppervlaktewaterpeilen worden afzonderlijk opgevraagd.

# Grondwater
#Peilen ophalen uit Watina. Alleen de gevalideerde gegevens worden meegenomen
#Verschillende keuze-mogelijkheden

#1. op basis van lijst verkregen via watina-package
# debugonce(import_watina_peilmetingen)
data_peilen_gw <- import_watina_peilmetingen(lijstmeetpunten = data_locs_ASH, ookdefinities = TRUE, referentie = "beide", toMenyanthes = FALSE)

#2. een gebiedscode

data_peilen_gw <- import_watina_peilmetingen(gebied = gebiedscode, ookdefinities = TRUE, referentie = "beide", toMenyanthes = FALSE, inclIngegeven = TRUE)

#3. een zelf samengestelde lijst van meetpunten
eigenlijstmeetpunten <- data.frame(meetpunten = c("VDBP004", "VDBP005"))
data_peilen_gw <- import_watina_peilmetingen(lijstmeetpunten = eigenlijstmeetpunten, ookdefinities = TRUE, referentie = "beide", toMenyanthes = FALSE)


#opsmuk tabel
colnames(data_peilen_gw) <- snakecase::to_snake_case(colnames(data_peilen_gw))
wt_peilpunten_gw <- wt_peilpunten
colnames(wt_peilpunten_gw) <- snakecase::to_snake_case(colnames(wt_peilpunten_gw))


#oppervlaktepeilen

#volledig analoog aan grondwater
data_peilen_ow <- import_watina_peilmetingen(gebied = gebiedscode, oppervlaktewater = TRUE, ookdefinities = TRUE, referentie = "beide", toMenyanthes = FALSE)
colnames(data_peilen_ow) <- snakecase::to_snake_case(colnames(data_peilen_ow))


wt_peilpunten_ow <- wt_peilpunten
colnames(wt_peilpunten_ow) <- snakecase::to_snake_case(colnames(wt_peilpunten_ow))

rm(wt_peilpunten)

#samenvoegen grond- en oppervlaktewaterdata
data_peilen <- bind_rows(data_peilen_gw %>% mutate(gw = 1), data_peilen_ow %>% mutate(gw = 0))
