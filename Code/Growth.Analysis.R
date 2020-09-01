library(ggplot2)
library(grid)
library(openxlsx)
library(officer)
library(rvg)
library(usmap)
library(httr)
library(cowplot)
library(dplyr)
library(tidyr)
library(bfw)
library(rnaturalearth)
library(sf)
library(zoo)
library(pracma)
library(scales)
library(rgeos)
library(maptools)
library(rgdal)
library(curl)
library(RPushbullet)
library(stringr)
#install.packages("cdccovidview", repos = c("https://cinc.rud.is", "https://cloud.r-project.org/"))
library(cdccovidview)
library(MMWRweek)
library(rvest)
library(geofacet)
#library(lubridate)
# rayshader is the package to put spikes on maps

remove(list=ls())
Directory <- "g:/projects/COVID/"
powerpnt <- '"C:/Program Files/Microsoft Office/root/Office16/powerpnt.exe"'
setwd(Directory)

# Specific directories
dirCode <- paste0(Directory,"Code/")
dirSheets <- paste0(Directory,"Sheets/")
dirUpdate <- paste0(Directory,"Updates/")
dirLatest <- paste0(Directory,"Latest/")

today <- Sys.Date()
weekDay <- as.POSIXlt(today)$wday
todayText <- as.character(today)
yesterday <- today - 1
yesterdayText <- format(yesterday, "X%m.%e.%y")
yesterdayText <- gsub("X0","X",yesterdayText)
yesterdayText <- gsub(" ","",yesterdayText) 
startDate <- as.Date("2020-01-22")
endDate <- as.Date("2020-10-01")
projection <- endDate - today + 1
currentDates <- seq(startDate, today-1, by="days")
allDates     <- seq(startDate, endDate, by="days")
allCurrentDays <- as.numeric(today-startDate)

dirTodayUpdate <- paste0(dirUpdate,todayText,"/")
dirTodayUpdateData <- paste0(dirTodayUpdate,"DATA/")


  source(paste0(dirCode,"Helper Functions.R"))
  source(paste0(dirCode,"calcStats.R"))

  source(paste0(dirCode,"persistent.download.R")) # Won't return until there are data for today
  source(paste0(dirCode,"ImportCovidData.R"))     # Reads files created from persistent.Download
  source(paste0(dirCode,"plotGrowth.R"))

  # Start main routine. Bracket required  for the stop command below to work.

  # Check that it is updated....

  lastColumn_USA <- grep(yesterdayText, names(Cases_USA))
  Cases_USA  <- Cases_USA[,1:lastColumn_USA]
  Deaths_USA <- Deaths_USA[,1:lastColumn_USA]

  lastColumn_Global <- grep(yesterdayText,names(Cases_Global))
  Cases_Global  <- Cases_Global[,1:lastColumn_Global]
  Deaths_Global <- Deaths_Global[,1:lastColumn_Global]
  
  # Needed for calcStates. Not actually needed for the growth analysis
  daysGompertzFit <- 21 # Number of days for the Gompertz model
  asymptomatic <- 10    # Number of asymptomatic patients per symptomatic patient,
  # limits Gompertz peak to population / asymptomatic * 0.6
  daysLinearFitCases  <- 14    # Number of days for the linear regression (increasing/decreasing cases per day)
  daysLinearFitDeaths <- 14    # Number of days for the linear regression (increasing/decreasing cases per day)
  
  
  plotGrowthFlag <- TRUE
  smoothingInterval <- 21
  deathThreshold <- 25
  shadedDays <- 30
  dayCutoff <- 150
  eachPlot <- TRUE
  
#############################################################################################
  newSection("International Growth Rates")
  
  DATA <- NULL
  for (i in 1:nrow(Population_Global))
  {
    Deaths <- Deaths_Global[Deaths_Global$Country == Population_Global$Country[i], lastColumn_Global]
    if (Deaths >= 25)
    X <- plotGrowth(
      calcStats(Country = Population_Global$Country[i]),
      Location = Population_Global$Country[i],
      Title = paste0(
        Population_Global$Country[i], 
        " (Population: ",
        Population_Global$Population[i],
        ", Deaths: ",
        Deaths,
        " as of ",
        todayText,
        ")"
      )
    )
    if (!is.null(DATA))
    {
      DATA <- rbind(DATA, X)
    } else {
      DATA <- X
    }
  }
  
ggObject1 <- plotGrowthSummary(
    DATA,
    "Cases",
    "Country",
    "#F8766D"
  )
    
ggObject2 <- plotGrowthSummary(
  DATA,
  "Deaths",
  "Country",
  "#00BFC4"
)

growthPlotArrange(
  ggObject1,
  ggObject2,
  "International"
)

newSection("US Growth Rates")
DATA <- NULL

for (i in 1:nrow(States))
{
  results <- calcStats(State = States$Abbreviation[i])
  Deaths <- results$DEATHS$Actual[results$DEATHS$Date == yesterday]
  if (Deaths >= 25)
  {
    X <- plotGrowth(
      results,
      Location = States$State[i],
      Title = paste0(
        States$State[i], 
        " (Population: ",
        results$Population,
        ", Deaths: ",
        Deaths,
        " as of ",
        todayText,
        ")"
      )
    )
    if (!is.null(DATA))
    {
      DATA <- rbind(DATA, X)
    } else {
      DATA <- X
    }
  }
  
  }
  
ggObject1 <- plotGrowthSummary(
  DATA,
  "Cases",
  "US State",
  "#F8766D"
)

ggObject2 <- plotGrowthSummary(
  DATA,
  "Deaths",
  "US State",
  "#00BFC4"
)

growthPlotArrange(
  ggObject1,
  ggObject2,
  "US States"
)
