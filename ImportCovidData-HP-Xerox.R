
if (!exists("today"))
{
  setwd("g:/projects/covid")
  today <- Sys.Date()
  startDate <- as.Date("2020-01-22")
  allCurrentDays <- as.numeric(today-startDate)
  currentDates <- seq(startDate, today-1, by="days")
  library(rnaturalearth)
  library(usmap)
  library(tidyr)
  library(rgeos)
  library(maptools)
  library(rgdal)
}

# Load Data
# USA
# Organization of files
# Country,

# Directory <- "g:/projects/COVID"
# today <- Sys.Date() 
# yesterday <- today - 1
# todayText <- as.character(today)


setwd(Directory)

###########################################################
### Read raw Data                                       ###
###########################################################

setwd(paste0(todayText,"/DATA"))

Cases_USA       <- read.csv(paste0("Cases_USA."     , todayText, ".raw.csv"))
Deaths_USA      <- read.csv(paste0("Deaths_USA."    , todayText, ".raw.csv"))
Testing_USA     <- read.csv(paste0("Testing_USA."   , todayText, ".raw.csv"))
Cases_Global    <- read.csv(paste0("Cases_Global."  , todayText, ".raw.csv"))
Deaths_Global   <- read.csv(paste0("Deaths_Global." , todayText, ".raw.csv"))
Testing_Global  <- read.csv(paste0("Testing_Global.", todayText, ".raw.csv"))
Mobility        <- read.csv(paste0("Mobility."      , todayText, ".raw.csv"))
Cases_Global$Province.State[Cases_Global$Country.Region == "Australia"] 
setwd(Directory)

###########################################################
### USA Data                                            ###
###########################################################

Population_USA <- read.csv("Population_USA.csv")
States <- read.csv("States.csv")
Counties <- read.csv("Counties.csv")

# The name of CountyFIPS in both files is bizarre, changing it to "CountyFIPS"
names(Cases_USA)[1] <- names(Deaths_USA)[1] <- "CountyFIPS"

# The Wade Hampton Census Area in Cases_USA and Deaths_USA (FIPS 2270) does not show up in the census. This deletes it.
USE <- Cases_USA$CountyFIPS != 2270
Cases_USA <- Cases_USA[USE,]
Deaths_USA <- Deaths_USA[USE,]

# The county called "New York City Unallocated" in Cases_USA and Deaths_USA accounts for very few cases.
# I merged it into "New York County"
NYC  <- which(Cases_USA$County.Name == "New York County")
NYCU <- which(Cases_USA$County.Name == "New York City Unallocated")
if (length(NYCU) > 0)
{
  Cases_USA[NYC, 5:ncol(Cases_USA)] <-
  Cases_USA[NYC , 5:ncol(Cases_USA)] +
  Cases_USA[NYCU, 5:ncol(Cases_USA)]

  Deaths_USA[NYC, 5:ncol(Deaths_USA)] <-
  Deaths_USA[NYC , 5:ncol(Deaths_USA)] +
  Deaths_USA[NYCU, 5:ncol(Deaths_USA)]

  Cases_USA <- Cases_USA[-NYCU,]
  Deaths_USA <- Deaths_USA[-NYCU,]
}
# The county named "Statewide Unallocated" has a countyFIPS of 0. Need to change that to 1000 * stateFIPS
USE <- Cases_USA$CountyFIPS == 0
Cases_USA$CountyFIPS[USE] <- Cases_USA$stateFIPS[USE] * 1000
Deaths_USA$CountyFIPS[USE] <- Deaths_USA$stateFIPS[USE] * 1000

# Verify matching
Cases_USA <- Cases_USA[order(Cases_USA$CountyFIPS),]
Deaths_USA <- Deaths_USA[order(Deaths_USA$CountyFIPS),]
Population_USA <- Population_USA[order(Population_USA$CountyFIPS),]

if (sum(Cases_USA$CountyFips != Deaths_USA$CountyFIPS) > 0)
    cat("Matching failed between allCases and allDeaths\n")
if (sum(Cases_USA$CountyFips != Population_USA$CountyFIPS) > 0)
  cat("Matching failed between allCases and Population\n")

# Testing in the US
Testing_USA$Date <- as.Date(as.character(Testing_USA$date), format = "%Y%m%d")
Testing_USA_temp <- cbind(
  States,
  matrix(0,nrow=51, ncol=length(currentDates))
)
names(Testing_USA_temp) <- c(names(States), as.character(currentDates, format = "X%m.%d.%y"))
for (i in 1:nrow(Testing_USA_temp))
{
  USE <- Testing_USA$state == Testing_USA_temp$Abbreviation[i]
  Testing_USA_temp[i, 4:ncol(Testing_USA_temp)] <- approx(
    Testing_USA$Date[USE], 
    Testing_USA$total[USE], 
    xout=currentDates, 
    yleft = 0,
    rule = 2
  )$y
}
Testing_USA <- Testing_USA_temp


###########################################################
### Global Data                                         ###
###########################################################

Population_Global <- read.csv("Population_Global.csv")

# Need to align country names with world map names
load("mapWorld.rData")
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
  Cases_Global$Country.Region[Cases_Global$Country.Region == Current_Names[i]] <- Revised_Names[i]
  Deaths_Global$Country.Region[Deaths_Global$Country.Region == Current_Names[i]] <- Revised_Names[i]
}
# Remove ship
USE <- Cases_Global$Country.Region != "ship"
Cases_Global <-   Cases_Global[USE,]
Deaths_Global <-   Deaths_Global[USE,]

# Verify that Cases_Global$Country.Region are accounted for in mapWorld
if (sum(!Cases_Global$Country.Region %in% mapWorld$geounit) > 0)
{
  cat("One of the countries in Cases_Global cannot be found in mapWorld$geounit\n")
  Cases_Global$Country.Region[!Cases_Global$Country.Region %in% mapWorld$geounit]
}

# Condense cases
names <- names(Cases_Global)[5:ncol(Cases_Global)]
E <- as.data.frame(
  matrix(
  0,
  ncol = length(names), 
  nrow = nrow(Population_Global)
))
names(E) <- names

D <- C <- as.data.frame(
  cbind(
    Population_Global$Country, E))
names(D)[1] <- names(C)[1] <- "Country"
for (i in 1:nrow(C))
{
  C[i,2:ncol(C)] <-
    colSums(
      Cases_Global[
        Cases_Global$Country.Region == C$Country[i],
        c(5:ncol(Cases_Global))],
      na.rm=TRUE
      )
  D[i,2:ncol(D)] <-
    colSums(
      Deaths_Global[
        Deaths_Global$Country.Region == D$Country[i],
        c(5:ncol(Deaths_Global))],
      na.rm=TRUE
    )
}

Cases_Global <- C   # [,1:(allCurrentDays+1)]
Deaths_Global <- D # [,1:(allCurrentDays+1)]

# Global from "Our World in Data"
Testing_Global_temp <- Testing_Global
Testing_Global_temp$date <- as.Date(Testing_Global_temp$date)
Testing_Global_temp <- Testing_Global_temp[!is.na(Testing_Global_temp$total_tests),]
Testing_Global_temp <-  Testing_Global_temp[,
                                            c("iso_code", "location", "date", "total_cases",
                                              "total_deaths", "total_tests", "total_tests_per_thousand")]
iso_codes <- sort(unique(Testing_Global_temp$iso_code))
countries <- mapWorld$geounit[match(iso_codes, mapWorld$adm0_a3_is)]
Testing_Global <- data.frame(
  iso_code = iso_codes, 
  country = countries,
  stringsAsFactors = FALSE
  )
Testing_Global$country[Testing_Global$iso_code == "GBR"] <- "United Kingdom"
Testing_Global$country[Testing_Global$iso_code == "BEL"] <- "Belgium"
Testing_Global <- cbind(Testing_Global, matrix(0, nrow=length(iso_codes), ncol=allCurrentDays))
names(Testing_Global) <- c("iso_code", "country", as.character(currentDates, format = "X%m.%d.%y"))
for (i in 1:nrow(Testing_Global))
{
  USE <- Testing_Global_temp$iso_code == Testing_Global$iso_code[i]
  if (sum(USE) == 1)
  {
    Column <- Testing_Global_temp$date[USE] - startDate + 2
    Testing_Global[i, Column:ncol(Testing_Global)] <- Testing_Global_temp$total_tests[USE]
  } else {
    Testing_Global[i, 3:ncol(Testing_Global)] <- approx(
      Testing_Global_temp$date[USE], 
      Testing_Global_temp$total_tests[USE], 
      xout=currentDates, 
      yleft = 0,
      rule = 2
      )$y
  }
}


# Fix USA Testing
Testing_Global[
  Testing_Global$iso_code == "USA", 
  3:ncol(Testing_Global)
] <- as.numeric(
  colSums(
    Testing_USA[, c(4:ncol(Testing_USA))],
    na.rm=TRUE
  )
)

#############################################################
### Add Mobility Data                                     ###
#############################################################

Mobility <- Mobility[!is.na(Mobility$country_region_code), ]
Mobility[,6:11][is.na(Mobility[,6:11])] <- 0
Mobility$Mean <- round(rowMeans(Mobility[,6:11]),2)
Mobility$Date <- as.Date(Mobility$date)
Mobility <- Mobility[,c(1,2,3,4, 13, 12)]
names(Mobility) <- c("countryCode","countryName","Region1","Region2", "Date","Mean")
Mobility <- as.data.frame(
  pivot_wider(Mobility, id_cols = c(countryCode, countryName, Region1, Region2), names_from = Date, values_from = Mean)
)

# USA Mobility Data
Mobility_USA <- Mobility[Mobility$countryCode == "US",]
Mobility_USA <- Mobility_USA[,3:ncol(Mobility_USA)]
names(Mobility_USA)[c(1, 2)] <- c("State","County")
Mobility_USA$Key <- paste(Mobility_USA$State, Mobility_USA$County)

# Fix County Names to match Cases_USA counties
CrossWalk <- read.csv("Mobility.USA.CrossWalk.csv", stringsAsFactors = TRUE)
for (i in 1:nrow(CrossWalk))
{
  j <- which(Mobility_USA$State == CrossWalk$State[i] & Mobility_USA$County == CrossWalk$County.in.Google.Mobility[i])
  Mobility_USA$County[j] <- CrossWalk$County.in.Cases[i]
}

# Change States to the abbreviation
CROWS <- match(Mobility_USA$State, States$State)
Mobility_USA$Abbreviation <- States$Abbreviation[CROWS]

# Global Mobility Data *****************************************
Mobility_Global <- Mobility[Mobility$Region1 == "",]
Mobility_Global$Region1 <- Mobility_Global$Region2 <- NULL

# Fix Country Names to match Cases_Global countries
CountryCodes <- read.csv("Country Codes.csv", stringsAsFactors = FALSE)
CountryCodes$Character.2[CountryCodes$Name == "Namibia"] <- "NA" # Pretty funny - the code is "NA", which R reads is as NA
CROWS <- match(Mobility_Global$countryCode, CountryCodes$Character.2)
Mobility_Global$countryName <- CountryCodes$Name[CROWS]

names(Mobility_Global)[2] <- "Country"
Mobility_Global$countryCode <- NULL
Mobility_Global <- Mobility_Global[order(Mobility_Global$Country),]
write.csv(Mobility_Global, "Global Mobility.csv", row.names=FALSE)

# Countries in Cases_Global without Mobility data
CROWS <- match(Cases_Global$Country, Mobility_Global$Country)
sum(is.na(CROWS))
Cases_Global$Country[is.na(CROWS)]

# Countries in Mobility_Global that aren't matched to Cases_Global
CROWS <- match(Mobility_Global$Country, Cases_Global$Country)
sum(is.na(CROWS))
Mobility_Global$Country[is.na(CROWS)]

# "Aruba"       "Hong Kong"   "Puerto Rico" "Réunion"
# This are, indeed, missing. The likely explanation is that they have been assigned to other countries

Lean <- read.csv("County Lean.csv", stringsAsFactors = FALSE)

###########################################################
### Write formatted data                                ###
###########################################################

setwd(paste0(todayText,"/DATA"))

write.csv(Cases_USA,       paste0("Cases_USA.",       todayText, ".csv"))
write.csv(Deaths_USA,      paste0("Deaths_USA.",      todayText, ".csv"))
write.csv(Testing_USA,     paste0("Testing_USA.",     todayText, ".csv"))
write.csv(Mobility_USA,    paste0("Mobility.",        todayText, ".csv"))
write.csv(Cases_Global,    paste0("Cases_Global.",    todayText, ".csv"))
write.csv(Deaths_Global,   paste0("Deaths_Global." ,  todayText, ".csv"))
write.csv(Testing_Global,  paste0("Testing_Global.",  todayText, ".csv"))
write.csv(Mobility_Global, paste0("Mobility_Global.", todayText, ".csv"))

setwd(Directory)




