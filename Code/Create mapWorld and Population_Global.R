# Create mapWorld and Population_Global
# Considerable effort to alignr names of mapWorld with names in Cases_Global

library(rnaturalearth)
# Need to align country names with world map names
mapWorld <- ne_countries(
  scale='medium',
  type = 'map_units',
  returnclass = 'sf'
)

Cases_Global <- read.csv("G:/projects/covid/2020-06-01/DATA/Cases_Global.2020-06-01.csv")
# Fix country names
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
  "West Bank",
  "Guinea Bissau",
  "Myanmar",
  "ship",
  "England",
  "Serbia",
  "Flemish Region",
  "Antigua"
)

for (i in 1:length(Current_Names))
{
  Cases_Global$Country[Cases_Global$Country == Current_Names[i]] <- Revised_Names[i]
}

# Verify that Cases_Global$Country.Region are accounted for in mapWorld
if (sum(!Cases_Global$Country.Region %in% mapWorld$geounit) > 0)
{
  cat("One of the countries in Cases_Global cannot be found in mapWorld$geounit\n")
  Cases_Global$Country.Region[!Cases_Global$Country.Region %in% mapWorld$geounit]
}

missing <- which(!mapWorld$geounit %in% Cases_Global$Country)
mapWorld$geounit[missing]

# Obvious fixes
mapWorld$geounit[mapWorld$geounit == "Scotland"] <- "England"
mapWorld$geounit[mapWorld$geounit == "Wales"] <- "England"
mapWorld$geounit[mapWorld$geounit == "Jersey"] <- "England"
mapWorld$geounit[grep("British", mapWorld$geounit)] <- "England"
mapWorld$geounit[grep("S.A.R", mapWorld$geounit)] <- "China"
mapWorld$geounit[grep("Netherlands", mapWorld$geounit)] <- "Netherlands"
mapWorld$geounit[grep("Brussels", mapWorld$geounit)] <- "Flemish Region"
mapWorld$geounit[grep("Walloon Region", mapWorld$geounit)] <- "Flemish Region"
mapWorld$geounit[mapWorld$geounit == "Barbuda"] <- "Antigua"
mapWorld$geounit[mapWorld$geounit == "Puerto Rico"] <- "United States of America"
mapWorld$geounit[mapWorld$geounit == "Guam"] <- "United States of America"
mapWorld$geounit[grep("America", mapWorld$geounit)] <- "United States of America"
mapWorld$geounit[grep("United States", mapWorld$geounit)] <- "United States of America"
mapWorld$geounit[grep("French", mapWorld$geounit)] <- "France"
mapWorld$geounit[mapWorld$geounit == "Gaza"] <- "West Bank"
mapWorld$geounit[mapWorld$geounit == "Aland"] <- "Finland"
mapWorld$geounit[mapWorld$geounit == "Republic Srpska"] <- "Bosnia and Herzegovina"
mapWorld$geounit[mapWorld$geounit == "Zanzibar"] <- "Tanzania"
mapWorld$geounit[mapWorld$geounit == "Northern Island"] <- "England"


# Remove mapWorld references not found in Cases_Global
missing <- !mapWorld$geounit %in% Cases_Global$Country
mapWorld <- mapWorld[!missing,]

# Remove freaking ship
Cases_Global <- Cases_Global[Cases_Global$Country != "ship",]

# Save output
save(mapWorld, file = "mapWorld.rData")

# Create Population Sum
Population <- data.frame(
  Country = sort(unique(Cases_Global$Country)),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(Population))
{
  Population$Population[i] <-
    sum(mapWorld$pop_est[
      mapWorld$geounit == Cases_Global$Country[i]],
      na.rm=TRUE
    )
}
Population <- Population[Population$Population > 0,]

GEOMAP <- read.xlsx("GEOMAP.xlsx")
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Antigua and Barbuda"] <- "Antigua"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "United Kingdom"] <- "England"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "United Kingdom"] <- "England"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Belgium"] <- "Flemish Region"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Russian Federation"] <- "Russia"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Viet Nam"] <- "Vietnam"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Serbia"][1] <- "Kosovo"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "North Macedonia"] <- "Macedonia"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Guinea-Bissau"] <- "Guinea Bissau"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Cote d'Ivoire"] <- "Ivory Coast"
GEOMAP$ISO.Country[GEOMAP$ISO.Country == "Palestine"] <- "West Bank"

CROWS <- match(Population$Country, GEOMAP$ISO.Country)
Population$Abbreviation <- GEOMAP$ISO[CROWS]
Population <- Population[!is.na(CROWS),]
Population_Global <- Population

write.csv(Population, "Population_Global.csv", row.names=FALSE)
