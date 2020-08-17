# 
# if (!exists("today"))
# {
#   setwd("g:/projects/covid")
#   today <- Sys.Date()
#   startDate <- as.Date("2020-01-22")
#   allCurrentDays <- as.numeric(today-startDate)
#   currentDates <- seq(startDate, today-1, by="days")
#   library(rnaturalearth)
#   library(usmap)
#   library(tidyr)
#   library(rgeos)
#   library(maptools)
#   library(rgdal)
# }

# Load Data
# USA
# Organization of files
# Country,

# Directory <- "g:/projects/COVID"
# today <- Sys.Date()
# yesterday <- today - 1
# todayText <- as.character(today)


###########################################################
### Read raw Data                                       ###
###########################################################


Cases_USA           <- read.csv(paste0(dirTodayUpdateData,"Cases_USA."     , todayText, ".raw.csv"))
Deaths_USA          <- read.csv(paste0(dirTodayUpdateData,"Deaths_USA."    , todayText, ".raw.csv"))
Testing_USA.raw     <- read.csv(paste0(dirTodayUpdateData,"Testing_USA."   , todayText, ".raw.csv"))
Cases_Global.raw    <- read.csv(paste0(dirTodayUpdateData,"Cases_Global."  , todayText, ".raw.csv"))
Deaths_Global.raw   <- read.csv(paste0(dirTodayUpdateData,"Deaths_Global." , todayText, ".raw.csv"))
Testing_Global.raw  <- read.csv(paste0(dirTodayUpdateData,"Testing_Global.", todayText, ".raw.csv"))
#Mobility            <- read.csv(paste0(dirTodayUpdateData,"Mobility."      , todayText, ".raw.csv"))
Ensemble            <- read.csv(paste0(dirTodayUpdateData,"Ensemble."      , todayText, ".raw.csv"))

###########################################################
### USA Data                                            ###
###########################################################

Population_USA <- read.csv(paste0(dirSheets,"Population_USA.csv"))
States <- read.csv(paste0(dirSheets,"States.csv"))
Counties <- read.csv(paste0(dirSheets,"Counties.csv"))

# Fix Names
names(Cases_USA)[grep("Province_State", names(Cases_USA))] <- "State"
names(Deaths_USA)[grep("Province_State", names(Deaths_USA))] <- "State"
names(Cases_USA)[grep("FIPS", names(Cases_USA))] <- "CountyFIPS"
names(Deaths_USA)[grep("FIPS", names(Deaths_USA))] <- "CountyFIPS"
names(Cases_USA)[grep("Admin2", names(Cases_USA))] <- "County.Name"
names(Deaths_USA)[grep("Admin2", names(Deaths_USA))] <- "County.Name"
names(Cases_USA)[grep("Country_Region", names(Cases_USA))] <- "stateFIPS"
names(Deaths_USA)[grep("Country_Region", names(Deaths_USA))] <- "stateFIPS"
Cases_USA$iso2 <- Deaths_USA$iso2 <- NULL
Cases_USA$iso3 <- Deaths_USA$iso3 <- NULL
Cases_USA$Combined_Key <- Deaths_USA$Combined_Key <- NULL
Cases_USA$Lat <- Deaths_USA$Lat <- NULL
Cases_USA$Long_ <- Deaths_USA$Long_ <- NULL
Cases_USA$UID <- Deaths_USA$UID <- NULL
Cases_USA$code3 <- Deaths_USA$code3 <- NULL
Cases_USA$Population <- Deaths_USA$Population <- NULL

CROWS <- match(Cases_USA$State, States$State)
Cases_USA$State <- States$Abbreviation[CROWS]
Cases_USA$stateFIPS <- States$FIPS[CROWS]
Cases_USA <- Cases_USA[!is.na(CROWS),]

CROWS <- match(Deaths_USA$State, States$State)
Deaths_USA$State <- States$Abbreviation[CROWS]
Deaths_USA$stateFIPS <- States$FIPS[CROWS]
Deaths_USA <- Deaths_USA[!is.na(CROWS),]

# Fix county names
CROWS <- match(Cases_USA$CountyFIPS, Counties$FIPS)
Cases_USA$County.Name[!is.na(CROWS)] <- Counties$Name[CROWS[!is.na(CROWS)]]

CROWS <- match(Deaths_USA$CountyFIPS, Counties$FIPS)
Deaths_USA$County.Name[!is.na(CROWS)] <- Counties$Name[CROWS[!is.na(CROWS)]]

# Sort order to match
Cases_USA <- Cases_USA[order(Cases_USA$State, Cases_USA$County.Name),]
Deaths_USA <- Deaths_USA[order(Deaths_USA$State, Deaths_USA$County.Name),]
cat("Cases County Name != Deaths County Name", sum(Cases_USA$County.Name != Deaths_USA$County.Name), "\n")

# Code below required for USA Facts. May not be necessary for Hopkins data
if (FALSE)
{
  Ascending <- function (X)
  {
    suppressWarnings(
    X <- as.numeric(X)
    )
    X[is.na(X)] <- 0
    for (i in 2:length(X))
    {
      if (X[i] < X[i-1]) X[i] <- X[i-1]
    }
    return(X)
  }
  
  # 1 non-numeric entry....., line 1283 on 6/16/20.... Arrgh
  
  Cols <- 5:ncol(Cases_USA)
  suppressWarnings(
    for (Col in Cols)
    {
      Cases_USA[,Col] <- as.numeric(Cases_USA[,Col])
      Deaths_USA[,Col] <- as.numeric(Deaths_USA[,Col])
  
    }
  )
  for (i in 1:nrow(Cases_USA))
  {
    Cases_USA[i,Cols] <- Ascending(Cases_USA[i,Cols])
    Deaths_USA[i,Cols] <- Ascending(Deaths_USA[i,Cols])
  }
}

# # Verify matching
# Cases_USA <- Cases_USA[order(Cases_USA$CountyFIPS),]
# Deaths_USA <- Deaths_USA[order(Deaths_USA$CountyFIPS),]
# Population_USA <- Population_USA[order(Population_USA$CountyFIPS),]
#
# if (sum(Cases_USA$CountyFips != Deaths_USA$CountyFIPS) > 0)
#     cat("Matching failed between allCases and allDeaths\n")
# if (sum(Cases_USA$CountyFips != Population_USA$CountyFIPS) > 0)
#   cat("Matching failed between allCases and Population\n")
#
# Testing and hospitalizations in the US
Testing_USA.raw$Date <- as.Date(as.character(Testing_USA.raw$date), format = "%Y%m%d")
Testing_USA.raw$percentPositive <- Testing_USA.raw$positive / (Testing_USA.raw$negative + Testing_USA.raw$positive) * 100
Testing_USA <- cbind(
  States[,1:3],
  matrix(0,nrow=51, ncol=length(currentDates))
)
names(Testing_USA) <- c(names(States)[1:3], as.character(currentDates, format = "X%m.%d.%y"))
Testing_USA_zeroOne <- Testing_USA_Daily <- Positive_USA <- 
  Positive_USA_zeroOne <- Hospitalization_USA_zeroOne <- 
  Testing_USA

# Testing_USA$hospitalizedCumulative and Testing_USA$hospitalized are identical
Cols <- ncol(Testing_USA)
for (i in 1:nrow(Testing_USA))
{
  USE <- Testing_USA.raw$state == Testing_USA$Abbreviation[i]
  Y <- approx(
    Testing_USA.raw$Date[USE],
    Testing_USA.raw$total[USE],
    xout=currentDates,
    yleft = 0,
    rule = 2
  )$y
  L <- length(Y)
  Testing_USA[i, 4:Cols] <- Y
  Y <- c(0,Y[2:L]-Y[1:(L-1)])
  Y <- supsmu(1:L, Y)$y
  Testing_USA_zeroOne[i, 4:Cols] <- zeroOne(Y)

  Y <- approx(
    Testing_USA.raw$Date[USE],
    Testing_USA.raw$percentPositive[USE],
    xout=currentDates,
    yleft = 0,
    rule = 2
  )$y
  Positive_USA[i, 4:Cols] <- Y
  Positive_USA_zeroOne[i, 4:Cols] <- zeroOne(Y)

  if (sum(!is.na(Testing_USA.raw$hospitalizedCurrently[USE])) > 10)
  {
    Y <- approx(
      Testing_USA.raw$Date[USE],
      Testing_USA.raw$hospitalizedCurrently[USE],
      xout=currentDates,
      yleft = 0,
      rule = 2
    )$y
  } else {
    Y <- approx(
      Testing_USA.raw$Date[USE],
      Testing_USA.raw$hospitalizedCumulative[USE],
      xout=currentDates,
      yleft = 0,
      rule = 2
    )$y
    L <- length(Y)
    DELTA <- Y[2:L] - Y[1:(L- 1)]
    Y[2:L] <- DELTA
  }
  Y <- supsmu(1:length(Y),Y)$y
  Hospitalization_USA_zeroOne[i, 4:Cols] <- zeroOne(Y)
}

Testing_Long <- Testing_USA.raw # Long form for ggplot

Testing_USA_zeroOne <- pivot_longer(
  Testing_USA_zeroOne,
  cols = 4:Cols,
  names_to = "Date",
  values_to = "Testing"
)
Testing_USA_zeroOne$Date <- rep(currentDates,51)

Positive_USA_zeroOne <- pivot_longer(
  Positive_USA_zeroOne,
  cols = 4:Cols,
  names_to = "Date",
  values_to = "Positive"
)
Positive_USA_zeroOne$Date <- rep(currentDates,51)

Hospitalization_USA_zeroOne <- pivot_longer(
  Hospitalization_USA_zeroOne,
  cols = 4:Cols,
  names_to = "Date",
  values_to = "Hospitalizations"
)
Hospitalization_USA_zeroOne$Date <- rep(currentDates,51)


###########################################################
### Global Data                                         ###
###########################################################

Population_Global <- read.csv(paste0(dirSheets,"Population_Global.csv"))

# Need to align country names with world map names
load(paste0(dirSheets,"mapWorld.rData"))
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
  Cases_Global.raw$Country.Region[Cases_Global.raw$Country.Region == Current_Names[i]] <- Revised_Names[i]
  Deaths_Global.raw$Country.Region[Deaths_Global.raw$Country.Region == Current_Names[i]] <- Revised_Names[i]
}
# Remove ship
USE <- Cases_Global.raw$Country.Region != "ship"
Cases_Global.raw <-   Cases_Global.raw[USE,]
Deaths_Global.raw <-   Deaths_Global.raw[USE,]

# Verify that Cases_Global.raw$Country.Region are accounted for in mapWorld
if (sum(!Cases_Global.raw$Country.Region %in% mapWorld$geounit) > 0)
{
  cat("One of the countries in Cases_Global.raw cannot be found in mapWorld$geounit\n")
  Cases_Global.raw$Country.Region[!Cases_Global.raw$Country.Region %in% mapWorld$geounit]
}

# Condense cases
names <- names(Cases_Global.raw)[5:ncol(Cases_Global.raw)]
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
      Cases_Global.raw[
        Cases_Global.raw$Country.Region == C$Country[i],
        c(5:ncol(Cases_Global.raw))],
      na.rm=TRUE
      )
  D[i,2:ncol(D)] <-
    colSums(
      Deaths_Global.raw[
        Deaths_Global.raw$Country.Region == D$Country[i],
        c(5:ncol(Deaths_Global.raw))],
      na.rm=TRUE
    )
}

Cases_Global <- C   # [,1:(allCurrentDays+1)]
Deaths_Global <- D # [,1:(allCurrentDays+1)]

# Global from "Our World in Data"
Testing_Global_temp <- Testing_Global.raw
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

# Mobility <- Mobility[!is.na(Mobility$country_region_code), ]
# Mobility[,6:11][is.na(Mobility[,8:13])] <- 0
# Mobility$Mean <- round(rowMeans(Mobility[,8:13]),2)
# Mobility$Date <- as.Date(Mobility$date)
# Mobility <- Mobility[,c(1,2,3,4, 14, 15)]
# names(Mobility) <- c("countryCode","countryName","Region1","Region2", "Date","Mean")
# Mobility <- as.data.frame(
#   pivot_wider(Mobility, id_cols = c(countryCode, countryName, Region1, Region2), names_from = Date, values_from = Mean)
# )

# # USA Mobility Data
# Mobility_USA <- Mobility[Mobility$countryCode == "US",]
# Mobility_USA <- Mobility_USA[,3:ncol(Mobility_USA)]
# names(Mobility_USA)[c(1, 2)] <- c("State","County")
# Mobility_USA$Key <- paste(Mobility_USA$State, Mobility_USA$County)
#
# # Fix County Names to match Cases_USA counties
# CrossWalk <- read.csv("Mobility.USA.CrossWalk.csv", stringsAsFactors = TRUE)
# for (i in 1:nrow(CrossWalk))
# {
#   j <- which(Mobility_USA$State == CrossWalk$State[i] & Mobility_USA$County == CrossWalk$County.in.Google.Mobility[i])
#   Mobility_USA$County[j] <- CrossWalk$County.in.Cases[i]
# }
#
# # Change States to the abbreviation
# CROWS <- match(Mobility_USA$State, States$State)
# Mobility_USA$Abbreviation <- States$Abbreviation[CROWS]
#
# # Global Mobility Data *****************************************
# Mobility_Global <- Mobility[Mobility$Region1 == "",]
# Mobility_Global$Region1 <- Mobility_Global$Region2 <- NULL
#
# # Fix Country Names to match Cases_Global countries
# CountryCodes <- read.csv("Country Codes.csv", stringsAsFactors = FALSE)
# CountryCodes$Character.2[CountryCodes$Name == "Namibia"] <- "NA" # Pretty funny - the code is "NA", which R reads is as NA
# CROWS <- match(Mobility_Global$countryCode, CountryCodes$Character.2)
# Mobility_Global$countryName <- CountryCodes$Name[CROWS]
#
# names(Mobility_Global)[2] <- "Country"
# Mobility_Global$countryCode <- NULL
# Mobility_Global <- Mobility_Global[order(Mobility_Global$Country),]
# write.csv(Mobility_Global, "Global Mobility.csv", row.names=FALSE)
#
# # Countries in Cases_Global without Mobility data
# CROWS <- match(Cases_Global$Country, Mobility_Global$Country)
# sum(is.na(CROWS))
# Cases_Global$Country[is.na(CROWS)]
#
# # Countries in Mobility_Global that aren't matched to Cases_Global
# CROWS <- match(Mobility_Global$Country, Cases_Global$Country)
# sum(is.na(CROWS))
# Mobility_Global$Country[is.na(CROWS)]
#
# # "Aruba"       "Hong Kong"   "Puerto Rico" "R?union"
# # This are, indeed, missing. The likely explanation is that they have been assigned to other countries

Lean <- read.csv(paste0(dirSheets,"County Lean.csv"), stringsAsFactors = FALSE)

# Process Ensemble model

# Limit to 95% prediction interval
Ensemble <- Ensemble[!is.na(Ensemble$quantile), ]
Ensemble <- Ensemble[Ensemble$type == "quantile", ]
Ensemble <- Ensemble[grep("cum death", Ensemble$target),]
Ensemble <- Ensemble[Ensemble$quantile == 0.010 | Ensemble$quantile == 0.990, ]
#Ensemble[Ensemble$location == "US",]

# Remove uninformative fields
Ensemble$forecast_date <- Ensemble$target <- Ensemble$ type<- NULL

# Fix locations
Ensemble <- Ensemble[!Ensemble$location %in% c("60", "66", "69", "72", "78"), ]
suppressWarnings(
  Ensemble$location <- as.numeric(Ensemble$location)
)
CROWS <- match(Ensemble$location, States$FIPS)
Ensemble$Location <- States$Abbreviation[CROWS]
Ensemble$Location[is.na(Ensemble$location)] <- "US"
Ensemble$location <- NULL

# Fix Date
Ensemble$Date <- as.Date(Ensemble$target_end_date)
Ensemble$target_end_date <- NULL

# Pivot
Ensemble <- pivot_wider(
  Ensemble,
  names_from = quantile,
  names_prefix = "q.",
  values_from = value
)

###########################################################
### Write formatted data                                ###
###########################################################

write.csv(Cases_USA,    paste0(dirTodayUpdateData, "Cases_USA.",       todayText, ".csv"), row.names = FALSE)
write.csv(Deaths_USA,   paste0(dirTodayUpdateData, "Deaths_USA.",      todayText, ".csv"), row.names = FALSE)
write.csv(Testing_USA,  paste0(dirTodayUpdateData, "Testing_USA.",     todayText, ".csv"), row.names = FALSE)
write.csv(Hospitalization_USA_zeroOne, paste0(dirTodayUpdateData, "Hospitalization_USA.",     todayText, ".csv"), row.names = FALSE)
#write.csv(Mobility_USA,    paste0(dirTodayUpdateData, "Mobility.",        todayText, ".csv"), row.names = FALSE)
write.csv(Cases_Global, paste0(dirTodayUpdateData, "Cases_Global.",    todayText, ".csv"), row.names = FALSE)
write.csv(Deaths_Global,  paste0(dirTodayUpdateData, "Deaths_Global." ,  todayText, ".csv"), row.names = FALSE)
write.csv(Testing_Global, paste0(dirTodayUpdateData, "Testing_Global.",  todayText, ".csv"), row.names = FALSE)
#write.csv(Mobility_Global, paste0(paste0(dirTodayUpdateData,"Mobility_Global.", todayText, ".csv"), row.names = FALSE)
write.csv(Ensemble,       paste0(dirTodayUpdateData, "Ensemble.",  todayText, ".csv"), row.names = FALSE)

