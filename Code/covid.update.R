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
todayText <- as.character(today)
yesterday <- today - 1
yesterdayText <- format(yesterday, "X%m.%e.%y")
yesterdayText <- gsub("X0","X",yesterdayText)
yesterdayText <- gsub(" ","",yesterdayText) 
startDate <- as.Date("2020-01-22")
endDate <- as.Date("2020-08-31")
projection <- endDate - today + 1
currentDates <- seq(startDate, today-1, by="days")
allDates     <- seq(startDate, endDate, by="days")
allCurrentDays <- as.numeric(today-startDate)

dirTodayUpdate <- paste0(dirUpdate,todayText,"/")
dirTodayUpdateData <- paste0(dirTodayUpdate,"DATA/")

daysLinearFitCases  <- 14    # Number of days for the linear regression (increasing/decreasing cases per day)
daysLinearFitDeaths <- 14    # Number of days for the linear regression (increasing/decreasing cases per day)
daysGompertzFit <- 21 # Number of days for the Gompertz model
asymptomatic <- 10    # Number of asymptomatic patients per symptomatic patient,
                      # limits Gompertz peak to population / asymptomatic * 0.6
deathAxis <- 20       # Relative size of right axis (deaths / day) to left axis (cases / day)

#while (TRUE)
#{
  
  EVENTS <- read.xlsx(paste0(dirSheets, "Events.xlsx"))
  EVENTS$Date <- as.Date(EVENTS$Date)
  EVENTS <- EVENTS[EVENTS$Date < today,]
  
  source(paste0(dirCode,"Helper Functions.R"))
  source(paste0(dirCode,"calcStats.R"))
  source(paste0(dirCode,"plotPred.R"))
  source(paste0(dirCode,"FisherPlots.R"))
  
  source(paste0(dirCode,"persistent.download.R")) # Won't return until there are data for today
  source(paste0(dirCode,"ImportCovidData.R"))     # Reads files created from persistent.Download

  # Start main routine. Bracket required  for the stop command below to work.

  # Check that it is updated....

  lastColumn_USA <- grep(yesterdayText, names(Cases_USA))
  Cases_USA  <- Cases_USA[,1:lastColumn_USA]
  Deaths_USA <- Deaths_USA[,1:lastColumn_USA]

  lastColumn_Global <- grep(yesterdayText,names(Cases_Global))
  Cases_Global  <- Cases_Global[,1:lastColumn_Global]
  Deaths_Global <- Deaths_Global[,1:lastColumn_Global]
  
#############################################################################################

  source(paste0(dirCode,"Summary.R"))
  source(paste0(dirCode,"States.R"))
  source(paste0(dirCode,"Counties.R"))
  source(paste0(dirCode,"International.R"))
  
  write.csv(Countries, paste0(dirTodayUpdateData, "Countries.csv"), row.names=FALSE)
  write.csv(States, paste0(dirTodayUpdateData, "States.csv"), row.names=FALSE)
  write.csv(Counties, paste0(dirTodayUpdateData, "Counties.csv"), row.names=FALSE)
  writeLines(emailText, paste0(dirTodayUpdate,"EmailText.",timestamp,".txt"))
  
  # Make copy of latest files for GitHub
  FILES <- list.files(dirLatest, recursive = TRUE)
  if (length(FILES) > 0) file.remove(paste0(dirLatest, FILES))
  
  FILES <- list.files(dirTodayUpdate, recursive = TRUE)
  file.copy(
    from = paste0(dirTodayUpdate,FILES),
    to = dirLatest
    )

  pbPost(
    #  devices = "Phone",
    channel = "",
    type = "note",
    title = "Message from R",
    body = "The report is complete.",
    apikey = "o.Jb1UN5cEOOZnaZ7Tp3rsxf4vShe82xXy"
  )

#}
