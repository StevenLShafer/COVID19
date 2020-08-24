# Create mapWorld and Population_Global
# Considerable effort to alignr names of mapWorld with names in JHU_Global

library(rnaturalearth)
# Need to align country names with world map names
mapWorld <- ne_countries(
  scale='medium',
  type = 'map_units',
  returnclass = 'sf'
)

JHU_Global <- read.csv(
  "G:/projects/covid/updates/2020-08-01/DATA/Cases_Global.2020-08-01.raw.csv"
  )[,c("Province.State", "Country.Region")]# Fix country names
names(JHU_Global) <- c("Province","Country")
# Preserve French Guiana
JHU_Global$Country[JHU_Global$Province == "French Guiana"] <- "French Guiana"
Current_Names <- c(
  "Bahamas",
  "Cabo Verde",
  "Congo (Brazzaville)",
  "Congo (Kinshasa)",
  "Cote d'Ivoire",
  "Diamond Princess",
  "Czechia",
  "Eswatini",
  "Holy See",
  "Korea, South",
  "North Macedonia",
  "Serbia",
  "Taiwan*",
  "US",
  "Timor-Leste",
  "West Bank and Gaza",
  "Guinea-Bissau",
  "Burma",
  "MS Zaandam",
  "United Kingdom",
  "Republic of Serbia",
  "Belgium",
  "Antigua and Barbuda"
)

Revised_Names <- c(
  "The Bahamas",
  "Cape Verde",
  "Republic of Congo",
  "Democratic Republic of the Congo",
  "Ivory Coast",
  "ship",
  "Czech Republic",
  "Swaziland",
  "Vatican",
  "South Korea",
  "Macedonia",
  "Republic of Serbia",
  "Taiwan",
  "United States of America",
  "East Timor",
  "West Bank and Gaza",
  "Guinea Bissau",
  "Myanmar",
  "ship",
  "United Kingdom",
  "Serbia",
  "Belgium",
  "Antigua and Barbuda"
)

for (i in 1:length(Current_Names))
{
  JHU_Global$Country[JHU_Global$Country == Current_Names[i]] <- Revised_Names[i]
}

# Verify that JHU_Global$Country.Region are accounted for in mapWorld
if (sum(!JHU_Global$Country.Region %in% mapWorld$geounit) > 0)
{
  cat("One or more of the countries in JHU_Global cannot be found in mapWorld$geounit\n")
  print(JHU_Global$Country.Region[!JHU_Global$Country.Region %in% mapWorld$geounit])
}

missing <- which(!mapWorld$geounit %in% JHU_Global$Country)
mapWorld$geounit[missing]

# Obvious fixes
mapWorld$geounit[mapWorld$geounit == "Bermuda"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Scotland"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Wales"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Jersey"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "England"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Northern Ireland"] <- "United Kingdom"
mapWorld$geounit[grep("British", mapWorld$geounit)] <- "United Kingdom"
mapWorld$geounit[grep("S.A.R", mapWorld$geounit)] <- "China"
mapWorld$geounit[grep("Netherlands", mapWorld$geounit)] <- "Netherlands"
mapWorld$geounit[grep("Brussels", mapWorld$geounit)] <- "Belgium"
mapWorld$geounit[grep("Walloon Region", mapWorld$geounit)] <- "Belgium"
mapWorld$geounit[grep("Flemish Region", mapWorld$geounit)] <- "Belgium"
mapWorld$geounit[mapWorld$geounit == "Puerto Rico"] <- "United States of America"
mapWorld$geounit[mapWorld$geounit == "Guam"] <- "United States of America"
mapWorld$geounit[mapWorld$geounit == "Northern Mariana Islands"] <- "United States of America"
mapWorld$geounit[grep("America", mapWorld$geounit)] <- "United States of America"
mapWorld$geounit[grep("United States", mapWorld$geounit)] <- "United States of America"
mapWorld$geounit[mapWorld$geounit == "Gaza"] <- "West Bank and Gaza"
mapWorld$geounit[mapWorld$geounit == "West Bank"] <- "West Bank and Gaza"
mapWorld$geounit[mapWorld$geounit == "Aland"] <- "Finland"
mapWorld$geounit[mapWorld$geounit == "Republic Srpska"] <- "Bosnia and Herzegovina"
mapWorld$geounit[mapWorld$geounit == "Zanzibar"] <- "Tanzania"
mapWorld$geounit[mapWorld$geounit == "Somaliland"] <- "Somalia"
mapWorld$geounit[mapWorld$geounit == "Aruba"] <- "Netherlands"
mapWorld$geounit[mapWorld$geounit == "Antigua"] <- "Antigua and Barbuda"
mapWorld$geounit[mapWorld$geounit == "Barbuda"] <- "Antigua and Barbuda"
mapWorld$geounit[mapWorld$geounit == "Guadeloupe"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Martinique"] <- "France"
mapWorld$geounit[mapWorld$geounit == "New Caledonia"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Azores"] <- "Portugal"
mapWorld$geounit[mapWorld$geounit == "Madeira"] <- "Portugal"
mapWorld$geounit[mapWorld$geounit == "Wallis and Futuna"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Reunion"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Turks and Caicos Islands"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Anguilla"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Falkland Islands"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Pitcairn Islands"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Cayman Islands"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Isle of Man"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Montserrat"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Guernsey"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Saint Helena"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Svalbard"] <- "Norway"
mapWorld$geounit[mapWorld$geounit == "Jan Mayen"] <- "Norway"
mapWorld$geounit[mapWorld$geounit == "South Georgia and South Sandwich Islands"] <- "United Kingdom"
mapWorld$geounit[mapWorld$geounit == "Norfolk Island"] <- "Australia"
mapWorld$geounit[mapWorld$geounit == "Ashmore and Cartier Islands"] <- "Australia"
mapWorld$geounit[mapWorld$geounit == "Cocos (Keeling) Islands"] <- "Australia"
mapWorld$geounit[mapWorld$geounit == "Christmas Island"] <- "Australia"
mapWorld$geounit[mapWorld$geounit == "Heard Island and McDonald Islands"] <- "Australia"
mapWorld$geounit[mapWorld$geounit == "Niue"] <- "New Zealand"
mapWorld$geounit[mapWorld$geounit == "Cook Islands"] <- "New Zealand"
mapWorld$geounit[mapWorld$geounit == "Greenland"] <- "Denmark"
mapWorld$geounit[mapWorld$geounit == "Sint Maarten"] <- "Netherlands"
mapWorld$geounit[mapWorld$geounit == "Curaçao"] <- "Netherlands"
mapWorld$geounit[mapWorld$geounit == "Saint Martin"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Saint Barthelemy"] <- "France"
mapWorld$geounit[mapWorld$geounit == "French Polynesia"] <- "France"
mapWorld$geounit[mapWorld$geounit == "French Southern and Antarctic Lands"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Saint Pierre and Miquelon"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Mayotte"] <- "France"
mapWorld$geounit[mapWorld$geounit == "Northern Cyprus"] <- "Cyprus"
mapWorld$geounit[mapWorld$geounit == "Faroe Islands"] <- "Denmark"
mapWorld$geounit[mapWorld$geounit == "Siachen Glacier"] <- "India"
mapWorld$geounit[mapWorld$geounit == "Bougainville"] <- "Papua New Guinea"
mapWorld$geounit[mapWorld$geounit == "Vojvodina"] <- "Serbia"
mapWorld$geounit[mapWorld$geounit == "Tokelau"] <- "New Zealand"

missing <- which(!mapWorld$geounit %in% JHU_Global$Country)
mapWorld$geounit[missing]

#X <- "Kos"
#JHU_Global[grepl(X, JHU_Global$Province) | grepl(X, JHU_Global$Country),]

# Remove mapWorld references not found in JHU_Global
# missing <- !mapWorld$geounit %in% JHU_Global$Country
# mapWorld <- mapWorld[!missing,]

# Remove freaking ship
JHU_Global <- JHU_Global[JHU_Global$Country != "ship",]

# Save output
save(mapWorld, file = "g:/projects/covid/sheets/mapWorld.rData")

# Create Population Sum
Population <- data.frame(
  Country = sort(unique(JHU_Global$Country)),
  Population = 0,
  stringsAsFactors = FALSE
)

for (i in 1:nrow(Population))
{
  Population$Population[i] <-
    sum(mapWorld$pop_est[
      mapWorld$geounit == Population$Country[i]],
      na.rm=TRUE
    )
}
Population <- Population[Population$Population > 0,]

GEOMAP <- read.xlsx("g:/projects/covid/sheets/GEOMAP.xlsx")
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Antigua and Barbuda"] <- "Antigua and Barbuda"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "United Kingdom"] <- "United Kingdom"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "United Kingdom"] <- "United Kingdom"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Belgium"] <- "Belgium"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Russian Federation"] <- "Russia"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Viet Nam"] <- "Vietnam"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Serbia"][1] <- "Kosovo"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "North Macedonia"] <- "Macedonia"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Guinea-Bissau"] <- "Guinea Bissau"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Cote d'Ivoire"] <- "Ivory Coast"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Palestine"] <- "West Bank and Gaza"
GEOMAP$ISO.Country[GEOMAP$Province == "French Guiana"] <- "French Guiana"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Vatican City"] <- "Vatican"

CROWS <- match(Population$Country, GEOMAP$ISO.Country)
Population[is.na(CROWS),]

Population$Abbreviation <- GEOMAP$ISO[CROWS]
Population_Global <- Population

write.csv(Population, "g:/projects/covid/sheets/Population_Global.csv", row.names=FALSE)
