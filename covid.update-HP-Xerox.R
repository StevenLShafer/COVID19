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

remove(list=ls())
Directory <- "g:/projects/COVID"
daysLinearFitCases  <- 5    # Number of days for the linear regression (increasing/decreasing cases per day)
daysLinearFitDeaths <- 14    # Number of days for the linear regression (increasing/decreasing cases per day)
daysGompertzFit <- 21 # Number of days for the Gompertz model
asymptomatic <- 10    # Number of asymptomatic patients per symptomatic patient, 
                      # limits Gompertz peak to population / asymptomatic * 0.6

setwd(Directory)

source("Setup PowerPoint.R")
source("Helper Functions.R")
source("calcStats.R")
source("plotPred.R")


#while (TRUE)
{
  today <- Sys.Date()
  todayText <- as.character(today)

  yesterday <- today - 1
  yesterdayText <- format(yesterday, "X%m.%e.%y")
  yesterdayText <- gsub("X0","X",yesterdayText)
  yesterdayText <- gsub(" ","",yesterdayText)
  
  startDate <- as.Date("2020-01-22")
  endDate <- as.Date("2020-06-30")
  projection <- endDate - today + 1
  currentDates <- seq(startDate, today-1, by="days")
  allDates     <- seq(startDate, endDate, by="days")
  allCurrentDays <- as.numeric(today-startDate)

  source("persistent.download.R") # Won't return until there are data for today  
  source("ImportCovidData.R")     # Reads files created from persistent.Download
  
  # Start main routine. Bracket required  for the stop command below to work. 
  
  # Check that it is updated....

  lastColumn <- grep(yesterdayText, names(Cases_USA),1)
  Cases_USA  <- Cases_USA[,1:lastColumn]
  Deaths_USA <- Deaths_USA[,1:lastColumn]
  
#############################################################################################
  plotPred(Country = "United States of America", Title = "USA")
  plotPred(Country = c("Japan","South Korea","Vietnam","Thailand"), Title = "Japan, South Korea, Thailand, Vietnam")
  plotPred(
    Country = c("England", "France", "Germany", "Greece", "Italy", "Portugal", "Spain"), 
    Title = "England, France, Germany, Greece, Italy, Portugal, and Spain"
  )
  # US Population is 318297903
  # Combined population for Japan, South Korea, Vietname, and Thailand is 328460585
  # Combined population of England, France, Germany, Greece, Italy, Portugal, and Spain is 324213323

  # State Summary
  States$Mortality <- States$deltaCases <- States$deltaDeaths <- States$dailyDeaths <- 0
  for (i in 1:nrow(States))
  {
    results <- calcStats(State = States$Abbreviation[i])
    if (!is.null(results))
    {
      States$deltaCases[i] <- results$slopeCases
      States$deltaDeaths[i] <- results$slopeDeaths
      States$Mortality[i] <- results$mortality
      States$Population[i] <- results$Population
      States$dailyDeaths[i] <- mean(results$DEATHS$Delta[results$DEATHS$Date < today & results$DEATHS$Date >= today-8 ])
    }
  }
  States$state <- States$State   # Required for plot_usmap().... whatever
  
  # New cases / day  
  States$signCase <- "No Change (-1% to +1%)"
  States$signCase[States$deltaCases < -3] <- "Decreasing > -3%"
  States$signCase[States$deltaCases >= -3 & States$deltaCases < -1] <- "Decreasing between -1% and -3%"
  States$signCase[States$deltaCases <= 3 & States$deltaCases > 1] <- "Increasing between +1% and +3%"
  States$signCase[States$deltaCases > 3] <- "Increasing > +3%"
  States$signCase <- factor(
    States$signCase, 
    levels = c(
      "Increasing > +3%",
      "Increasing between +1% and +3%",
      "No Change (-1% to +1%)",
      "Decreasing between -1% and -3%",
      "Decreasing > -3%")
  )
  
  ggObject <- plot_usmap(data = States, values = "signCase", color = "black") +
    scale_fill_manual(values = c("red", "pink", "white","lightgreen", "green"), name = "New cases are:", drop=FALSE) + 
    theme(legend.position = "right") +
    labs(
      title = paste("New cases by state as of", Sys.Date())
    )
  nextSlide(ggObject, "Change in New Cases per Day")
  
  # New deaths / day
  States$deltaDeaths[States$deltaDeaths < -6] <- -6 
  States$deltaDeaths[States$deltaDeaths > 6] <- 6 
  States$signDeath <- "No Change (-0.1% to +0.1%)"
  States$signDeath[States$deltaDeaths < -0.5] <- "Decreasing > -0.5%"
  States$signDeath[States$deltaDeaths >= -0.5 & States$deltaDeaths < -0.1] <- "Decreasing between -0.1% and -0.5%"
  States$signDeath[States$deltaDeaths <= 0.5 & States$deltaDeaths > 0.1] <- "Increasing between +0.1% and +0.5%"
  States$signDeath[States$deltaDeaths > 0.5] <- "Increasing > +0.5%"
  States$signDeath <- factor(
    States$signDeath, 
    levels = c(
      "Increasing > +0.5%",
      "Increasing between +0.1% and +0.5%",
      "No Change (-0.1% to +0.1%)",
      "Decreasing between -0.1% and -0.5%",
      "Decreasing > -0.5%")
  )
  
  # New deaths / day  
  ggObject <- plot_usmap(data = States, values = "signDeath", color = "black") +
    scale_fill_manual(values = c("red", "pink", "white","lightgreen", "green"), name = "New deaths are:", drop=FALSE) + 
    theme(legend.position = "right") +
    labs(
      title = paste("New deaths by state as of", Sys.Date())
    )
  nextSlide(ggObject, "Change in New Deaths per Day")
  
  # Fisher Plot  
  States$DeathsperMillion <- States$dailyDeaths  / States$Population * 1000000
  X <- States[order(States$DeathsperMillion, States$Abbreviation), ]
  X$Rank <- 1:51
  ggObject <- ggplot(X, aes(x = Rank, y = DeathsperMillion)) +
    geom_point() + 
    geom_text(aes(y = DeathsperMillion + 0.4, label = Abbreviation), angle = 90, size = 2.6) +
    labs(
      title = "US COVID-19 Death Rates",
      subtitle = "Averaged over past 7 days",
      y = "Average deaths per million population",
      x = "State"
    )
  nextSlide(ggObject, "Deaths per million")
  
  #########################################################################################
  # County Summary
  Counties$deltaCases <- 0
  Counties$deltaDeaths <- 0
  Counties$Population <- 0
  Counties$Cases <- 0
  Counties$Deaths <- 0
  Counties$Mortality <- 0
  for (i in 1:nrow(Counties))
  {
    x <- which(Cases_USA$CountyFIPS == Counties$FIPS[i])
    results <- calcStats(County = Cases_USA$County.Name[x], State = Cases_USA$State[x])
    if (!is.null(results))
    {
      Counties$deltaCases[i]  <- results$slopeCases
      Counties$deltaDeaths[i] <- results$slopeDeaths
      Counties$Cases[i]   <- results$yesterdayCases
      Counties$Deaths[i]  <- results$yesterdayDeaths
      Counties$Population[i] <- results$Population
      Counties$Mortality[i]  <- results$mortality
    }
  }
  Counties$fips <- Counties$FIPS
  Counties$deltaCases[is.na(Counties$deltaCases)] <- 0
  Counties$percentCases <- Counties$Cases / Counties$Population * 100
  Counties$percentDeaths <- Counties$Deaths / Counties$Population * 100
  Counties$signCase <- "No Change (-0.5% to +0.5%)"
  Counties$signCase[Counties$deltaCases < -2] <- "Decreasing > -2%"
  Counties$signCase[Counties$deltaCases >= -2 & Counties$deltaCases < -0.5] <- "Decreasing between -0.5% and -2%"
  Counties$signCase[Counties$deltaCases <= 2 & Counties$deltaCases > 0.5] <- "Increasing between +0.5% and +2%"
  Counties$signCase[Counties$deltaCases > 2] <- "Increasing > +2%"
  Counties$signCase <- factor(
    Counties$signCase, 
    levels = c(
      "Increasing > +2%",
      "Increasing between +0.5% and +2%",
      "No Change (-0.5% to +0.5%)",
      "Decreasing between -0.5% and -2%",
      "Decreasing > -2%")
  )
  # Add Partisan Lean
  CROWS <- match(Counties$FIPS, Lean$FIPS)
  Counties$Lean <- Lean$Lean[CROWS] * 100
  
  # New cases / day  
  ggObject <- plot_usmap(data = Counties, values = "signCase", color = "black") +
    scale_fill_manual(values = c("red", "pink", "white","lightgreen", "green"), name = "Direction") + 
    theme(legend.position = "right") +
    labs(
      title = paste("Trends by county as of", Sys.Date()),
      caption = "NA = Inadequate data"
    )
  nextSlide(ggObject, "Change in New Cases per Day")
  

  # Percent Change by Partisan Lean  
  subset <- Counties[
    abs(Counties$deltaCases) < 12 & 
      abs(Counties$deltaCases) > 0.01 & 
      !is.na(Counties$Lean),
    ]
  smooth <- supsmu(subset$Lean, subset$deltaCases)
  smooth <- data.frame(
    x = smooth$x,
    y = smooth$y
  )
  ggObject <- ggplot(subset,aes(x = Lean, y = deltaCases, color = Lean)) +
    geom_point(size = 0.85) +
    annotate("segment", x = 0, xend = 100, y = 0, yend = 0, color = "black") +
    geom_line(data = smooth, aes(x = x, y = y), color = "darkgreen", linetype = "solid", size = 1.5, alpha = 0.5) +
    scale_color_gradient2(low = "blue",mid = "purple", high="red", midpoint = 50) +
    scale_fill_gradient(low = "blue",high="red") +
    labs(
      x = "Percent Republican",
      y = "Percent change in new cases per day",
      title = "Counties by 2016 presidential election results",
      color = "Republican",
      caption = "Dark green line is a Friedman's supersmoother"
    ) +
    coord_cartesian(expand=FALSE, xlim = c(0,100), ylim = c(-13, 13))
  nextSlide(ggObject, "Percent Change by Partisan Lean")

  # Percent Change by Population
  subset <- Counties[
    abs(Counties$deltaCases) < 12 & 
      abs(Counties$deltaCases) > 0.01 & 
      Counties$Population >= 1000,
    ]
  smooth <- supsmu(subset$Population, subset$deltaCases)
  smooth <- data.frame(
    x = smooth$x,
    y = smooth$y
  )
  
  ggObject <- ggplot(subset,aes(x = Population, y = deltaCases, color = Lean)) +
    geom_point(size = 0.85) +
    annotate("segment", x = 0, xend = 10000000, y = 0, yend = 0, color = "black") +
    geom_line(data = smooth, aes(x = x, y = y), color = "darkgreen", linetype = "solid", size = 1.5, alpha = 0.5) +
    scale_color_gradient2(low = "blue",mid = "purple", high="red", midpoint = 50) +
    labs(
      x = "Population",
      y = "Percent change in new cases per day",
      title = "Counties by Population",
      color = "Republican",
      caption = "Dark green line is a Friedman's 'super smoother'"
    ) +
    coord_cartesian(expand=FALSE, xlim = c(1000, 12000000), ylim = c(-13, 13)) +
    scale_x_log10(
      breaks = c(1000, 10000, 100000, 1000000,10000000),
      labels = c("1,000","10,000","100,000","1,000,000", "10,000,000")
    ) 
  nextSlide(ggObject, "Percent Change by Population")
  
  # Epstein map
  subset <- Counties[
    abs(Counties$deltaCases) < 12 & 
      abs(Counties$deltaCases) > 0.01 & 
      Counties$Population >= 1000,
    ]
  smooth <- supsmu(subset$Lean, subset$Population)
  smooth <- data.frame(
    x = smooth$x,
    y = smooth$y
  )
  
  ggObject <- ggplot(subset,aes(x = Lean, y = Population, color = signCase)) +
    geom_point(size = 0.85) +
    geom_line(data = smooth, aes(x = x, y = y), color = "darkgreen", linetype = "solid", size = 1.5, alpha = 0.5) +
    scale_color_manual(values = c("red", "pink", "white","lightgreen", "green"), name = "Direction") +
    #scale_color_gradient2(low = "#02A602",mid = "white", high="#CD0101", midpoint = 0) +
    labs(
      x = "Percent Republican",
      y = "Population",
      title = "Partisan Lean vs Population and Direction",
      color = "Increase/Decrease",
      caption = "Dark green line is a Friedman's 'super smoother'"
    ) +
    coord_cartesian(expand=FALSE, xlim = c(0, 100), ylim=c(1000, 12000000) ) +
    scale_x_continuous(
      # breaks = c(1000, 10000, 100000, 1000000,10000000),
      # labels = c("1,000","10,000","100,000","1,000,000", "10,000,000")
    ) +
  scale_y_log10(
    breaks = c(1000, 10000, 100000, 1000000,10000000),
    labels = c("1,000","10,000","100,000","1,000,000", "10,000,000")
  ) +
  theme(
    panel.background = element_rect(
      fill = NA, 
      colour = NA, 
    ),
  panel.grid.major = element_line(colour = "grey"),
  legend.key = element_rect(fill = NA, colour = "white"),
  axis.line = element_line(color = "black") 
  ) +
  annotation_logticks(sides = "l") 
  nextSlide(ggObject, "Partisan Lean vs Population and Direction")
  
  # Percent Cases Population *************************************************************
  subset <- Counties[
     Counties$Population >= 1000,
   ]

  # Percent Cases
  smooth <- supsmu(subset$Population, subset$percentCases)
  smooth <- data.frame(
    x = smooth$x,
    y = smooth$y
  )
  ggObject <- ggplot(subset,aes(x = Population, y = percentCases)) +
    geom_point(size = 0.85) +
    geom_line(data = smooth, aes(x = x, y = y), color = "darkgreen", linetype = "solid", size = 1.5, alpha = 0.5) +
    labs(
      x = "County Population",
      y = "Total cases",
      title = "Total Cases as a Percent of County Population",
      caption = "Slanted lines are counties with small integer numbers of cases, green line: Friedman's 'super smoother'"
    ) +
    coord_cartesian(expand=FALSE, xlim = c(1000, 12000000), ylim = c(0.001, 20)) +
    scale_x_log10(
      breaks = c(1000, 10000, 100000, 1000000,10000000),
      labels = c("1,000","10,000","100,000","1,000,000", "10,000,000")
    ) +
    scale_y_log10(
      breaks = c(0.001, 0.01, 0.1, 1, 10, 20),
      labels = c("0.001%", "0.01%", "0.1%", "1%", "10%", "20%")
    ) +
    annotation_logticks() 
  nextSlide(ggObject, "Cases as a Percent of Population")
   
  # Percent Deaths
  smooth <- supsmu(subset$Population, subset$percentDeaths)
  smooth <- data.frame(
    x = smooth$x,
    y = smooth$y
  )
  ggObject <- ggplot(subset,aes(x = Population, y = percentDeaths)) +
    geom_point(size = 0.85) +
    geom_line(data = smooth, aes(x = x, y = y), color = "darkgreen", linetype = "solid", size = 1.5, alpha = 0.5) +
    labs(
      x = "County Population",
      y = "Total deaths",
      title = "Total Deaths as a Percent of County Population",
      caption = "Slanted lines are counties with small integer numbers of cases, green line: Friedman's 'super smoother'"
    ) +
    coord_cartesian(expand=FALSE, xlim = c(1000, 12000000), ylim = c(0.0001, 1)) +
    scale_x_log10(
      breaks = c(1000, 10000, 100000, 1000000,10000000),
      labels = c("1,000","10,000","100,000","1,000,000", "10,000,000")
    ) +
    scale_y_log10(
      breaks = c(0.0001, 0.001, 0.01, 0.1, 1),
      labels = c("0.0001%", "0.001%", "0.01%", "0.1%", "1%")
    ) +
    annotation_logticks() 
  nextSlide(ggObject, "Deaths as a Percent of Population")
   
  # Population vs. Mortality
  smooth <- supsmu(subset$Population, subset$Mortality)
  smooth <- data.frame(
    x = smooth$x,
    y = smooth$y
  )
  ggObject <- ggplot(subset,aes(x = Population, y = Mortality * 100)) +
    geom_point(size = 0.85) +
    geom_line(data = smooth, aes(x = x, y = y * 100), color = "darkgreen", linetype = "solid", size = 1.5, alpha = 0.5) +
    labs(
      x = "County Population",
      y = "Case mortality",
      title = "Case Mortality vs. County Population"
    ) +
    coord_cartesian(expand=FALSE, xlim = c(1000, 12000000), ylim = c(0.1, 100)) +
    scale_x_log10(
      breaks = c(1000, 10000, 100000, 1000000,10000000),
      labels = c("1,000","10,000","100,000","1,000,000", "10,000,000")
    ) +
    scale_y_log10(
      breaks = c(0.1, 1, 10, 100),
      labels = c("0.1%", "1%", "10%", "100%")
    ) +
    annotation_logticks() 
  nextSlide(ggObject, "Case Mortality vs. Population")
 
  # States ************************************************************************
  for (i in 1:nrow(States))
    plotPred(State = States$Abbreviation[i], Title = States$State[i])

  # Counties
  plotPred(
    County = c(
      "New York County",
      "Kings County",
      "Bronx County",
      "Queens County",
      "Richmond County"
    ),
    State = "NY",
    Title = "New York City"
  )
  plotPred(County = "Westchester County", Title = "Westchester County, NY")
  plotPred(County = "Bergen County", Title = "Bergen County, NJ")
  plotPred(County = "Hudson County", Title = "Hudson County, NJ")
  plotPred(County = "King County", State = "WA", Title = "King County (Seattle)")
  plotPred(County = "Los Angeles County", State = "CA", Title = "Los Angeles")
  plotPred(County = c("Santa Clara County", "San Mateo County", "San Francisco County", "Marin County", "Napa County", "Solano County", "Sonoma County"),
           Title = "Bay Area")
  plotPred(County = "San Francisco County", Title = "San Francisco")
  plotPred(County = c("Santa Clara County", "San Mateo County"), Title = "Santa Clara and San Mateo")
  plotPred(County = "Alameda County", Title = "Alameda County")
  plotPred(County = "San Luis Obispo County", Title = "San Luis Obispo, California")
  plotPred(County = "Fresno County", Title = "Fresno, California")
  plotPred(County = "Santa Barbara County", Title = "Santa Barbara County")
  plotPred(County = "Merced County", State = "CA", Title = "Merced County")
  plotPred(County = "Yolo County", State = "CA", Title = "Yolo County")
  plotPred(County = "Multnomah County", Title = "Multnomah County (Portland)")
  plotPred(County = "Utah County", Title = "Utah County")
  plotPred(County = "Summit County", State = "UT", Title = "Summit County, Utah")
  plotPred(County = "De Soto Parish", Title = "De Soto Parish, Louisiana")
  plotPred(County = "Dallas County", State = "TX", Title = "Dallas, Texas")
  plotPred(County = "Collin County", State = "TX", Title = "Collin Texas")
  plotPred(County = "Harris County", State = "TX", Title = "Harris County, Texas")
  plotPred(County = "McLean County", State = "IL", Title = "McLean County, Illinois")
  plotPred(County = "Cook County", State = "IL", Title = "Cook County, Illinois")
  plotPred(County = "Suffolk County", State = "MA", Title = "Suffolk County (Boston)")
  plotPred(County = "Polk County", State = "IA", Title = "Polk County, Iowa")
  plotPred(County = "Johnson County", State = "IA", Title = "Johnson County, Iowa")
  plotPred(County = "Erie County", State = "NY", Title = "Erie County, New York")
  plotPred(County = "Oakland County", State = "MI", Title = "Oakland County, Michigan")
  plotPred(County = "City of St. Louis", Title = "St. Louis (City)")
  plotPred(County = "St. Louis County", Title = "St. Louis (County)")
  plotPred(County = "Baltimore City", Title = "Baltimore (City)")
  plotPred(County = "Durham County", Title = "Durham County")
  plotPred(County = "Cuyahoga County", Title = "Cuyahoga County (Cleveland)")
  plotPred(County = "Wayne County", State = "MI", Title = "Wayne County (Detroit)")
  plotPred(County = "Miami-Dade County", Title = "Miami-Dade")
  plotPred(County = "Maricopa County", Title = "Maricopa County")
  plotPred(County = "Denver County", State = "CO", Title = "Denver County")
  plotPred(County = "Arapahoe County", State = "CO", Title = "Arapahoe County")
  plotPred(County = "Montrose County", State = "CO", Title = "Montrose County")
  
  # Testing vs. Mortality
  CROWS <- match(States$Abbreviation,Testing_USA$Abbreviation)
  States$Tests <- Testing_USA[CROWS, ncol(Testing_USA)]
  States$TestingFraction <- States$Tests / States$Population * 100

  # Testing
  ggObject <- ggplot(States,aes(x = TestingFraction, y = Mortality * 100, label=Abbreviation)) +
    geom_text(size = 3) +
    labs(
      title = paste("Mortality vs. Testing as of", today),
      y = "% Mortality",
      x = "% Tested"
    ) +
  coord_cartesian(ylim = c(0, 10), xlim = c(0, 15))

  nextSlide(ggObject, "Case Mortality vs. Testing")

  ###############################################################################################################
  # Worldwide
  plotPred(Country = "Italy", Title = "Italy")
  plotPred(Country = "Spain", Title = "Spain")
  plotPred(Country = "Portugal", Title = "Portugal")
  plotPred(Country = "France", Title = "France")
  plotPred(Country = "Germany", Title = "Germany")
  plotPred(Country = "Switzerland", Title = "Switzerland")
  plotPred(Country = "Austria", Title = "Austria")
  plotPred(Country = "Sweden", Title = "Sweden")
  plotPred(Country = "Netherlands", Title = "Netherlands")
  plotPred(Country = "England", Title = "United Kingdom")
  plotPred(Country = "Iceland", Title = "Iceland")
  plotPred(Country = "Israel", Title = "Israel")
  plotPred(Country = "Paraguay", Title = "Paraguay")
  # plotPred(Country = "Rwanda", Title = "Rwanda"
  plotPred(Country = "Canada", Title = "Canada")
  plotPred(Country = "Australia", Title = "Australia")
  plotPred(Country = "New Zealand", Title = "New Zealand")
  plotPred(Country = "South Korea", Title = "South Korea")
  plotPred(Country = "Singapore", Title = "Singapore")
  plotPred(Country = "Japan", Title = "Japan")
  plotPred(Country = "South Africa", Title = "South Africa")
  plotPred(Country = "Brazil", Title = "Brazil")
  plotPred(Country = "Russia", Title = "Russia")
  plotPred(Country = "India", Title = "India")
  plotPred(Country = "Mexico", Title = "Mexico")

  # World Map of Cases
  CROWS <- match(mapWorld$geounit, Cases_Global$Country)
  mapWorld$cases <- Cases_Global[CROWS, ncol(Cases_Global)]
  mapWorld$lcases <- log(mapWorld$cases)
  CROWS <- match(mapWorld$geounit, Deaths_Global$Country)
  mapWorld$deaths <- Deaths_Global[CROWS, ncol(Deaths_Global)]
  mapWorld$ldeaths <- log(mapWorld$deaths)
  mapWorld$mortality = mapWorld$deaths / mapWorld$cases * 100
  # Note: many locations have the same name, because it may include islands, etc. Thus, the
  # sum of cases >>> actual cases, because the same numbers are populated in all the locations
  mapWorld$mortality_limited <- pmin(mapWorld$mortality, 15) # Limit to 15% to not distort map below

  mapWorld <- mapWorld[!is.na(mapWorld$lcases),]

  ggObject <-   ggplot() +
      geom_sf(data = mapWorld, color="#00013E", aes(fill=lcases)) +
      scale_fill_gradient(low="#FFCFCF",high="#C00000") +
    labs(
      title = paste("Total Cases as of", Sys.Date())
    ) +
      theme(
        panel.background = element_rect(
          fill = "#00013E"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
      )
  nextSlide(ggObject, "Worldwide Distribution of Cases")

  ggObject <-   ggplot() +
    geom_sf(data = mapWorld, color="#00013E", aes(fill=ldeaths)) +
    scale_fill_gradient(low="#FFCFCF",high="#C00000") +
    labs(
      title = paste("Total Deaths as of", Sys.Date())
    ) +
    theme(
      panel.background = element_rect(
        fill = "#00013E"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
  nextSlide(ggObject, "Worldwide Distribution of Deaths")

  ggObject <-   ggplot() +
    geom_sf(data = mapWorld, color="#00013E", aes(fill=mortality_limited)) +
    scale_fill_gradient(low="#FFCFCF",high="#C00000") +
    labs(
      title = paste("Case mortality as of", Sys.Date()),
      fill = "Mortality (%)",
      caption = paste(
        "Overall case mortality: ",
        round(sum(Deaths_Global[,ncol(Deaths_Global)])/sum(Cases_Global[,ncol(Cases_Global)]) * 100, 1),
        "%")
    ) +
    theme(
      panel.background = element_rect(
        fill = "#00013E"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
#      legend.position = "none",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
  nextSlide(ggObject, "Worldwide Case Mortality")

  # Doubling Time by Country
  doubling_Global <- data.frame(
  Country = Cases_Global$Country,
  doublingTime = NA
  )

  cols <- (ncol(Cases_Global)-4):ncol(Cases_Global)
  X <- 1:5
  for (i in 1:nrow(doubling_Global))
  {
    if (Cases_Global[i,cols[1]] > 0)
    {
      Y <- unlist(log(Cases_Global[i,cols]))
      doubling_Global$doublingTime[i] <- 0.693 / lm(Y ~ X)$coefficients[2]
    }
  }
  doubling_Global <- doubling_Global[!is.na(doubling_Global$doublingTime),]
  doubling_Global <- doubling_Global[!is.infinite(doubling_Global$doublingTime),]
  doubling_Global$doublingTime <- pmax(doubling_Global$doublingTime, 0)
  doubling_Global$doublingTime <- pmin(doubling_Global$doublingTime, 68)
  doubling_Global$doublingTimeWorldMap <- pmin(doubling_Global$doublingTime, 30)

  CROWS <- match(mapWorld$geounit, doubling_Global$Country)
  mapWorld$doublingTimeWorldMap <- doubling_Global$doublingTimeWorldMap[CROWS]

  ggObject <-   ggplot() +
    geom_sf(data = mapWorld, color="#00013E", aes(fill=doublingTimeWorldMap)) +
    scale_fill_gradient(high="#FFCFCF",low="#C00000") +
    labs(
      title = paste("Doubling Time as of", Sys.Date())
    ) +
    theme(
      panel.background = element_rect(
        fill = "#00013E"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
 #     legend.position = "none",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
  nextSlide(ggObject, "Worldwide Doubling Time")

  # doubling_Global$Country <- factor(doubling_Global$Country, levels = doubling_Global$Country[order(doubling_Global$doublingTime)], ordered = TRUE)
  # Title <- paste("Doubling time over the last 5 days as of", Sys.Date())
  # 
  # ggObject <- ggplot(doubling_Global[doubling_Global$doublingTime <= 14,], aes(x=Country, y=doublingTime)) +
  #   geom_col(fill = "brown", color="black", width=.5) +
  #   coord_cartesian(
  #     ylim = c(0,14)) +
  #   scale_y_continuous(breaks = c(2,4,5,8,10,12,14)) +
  #   theme(axis.text.x=element_text(angle=90, hjust=1, size = 8)) +
  #   labs(
  #     title = Title,
  #     y = "Doubling Time",
  #     x = "Country"
  #   )
  # nextSlide(ggObject, "Doubling time (last 5 days)")

  # ggObject <- ggplot(doubling_Global[doubling_Global$doublingTime > 14,], aes(x=Country, y=doublingTime)) +
  #   geom_col(fill = "brown", color="black", width=.5) +
  #   coord_cartesian(
  #     ylim = c(14,68)) +
  #   scale_y_continuous(breaks = c(14, 28, 42, 54, 68)) +
  #   theme(axis.text.x=element_text(angle=90, hjust=1, size = 8)) +
  #   labs(
  #     title = Title,
  #     y = "Doubling Time",
  #     x = "Country"
  #   )
  # nextSlide(ggObject, "Doubling Time (last 5 days)")

# Testing
  Testing_Global_last <- Testing_Global_temp[order(Testing_Global_temp$date, decreasing = TRUE),]
  Testing_Global_last <- Testing_Global_last[!duplicated(Testing_Global_last$iso_code),]
  Testing_Global_last$mortality <- Testing_Global_last$total_deaths / Testing_Global_last$total_cases * 100
  Testing_Global_last <- Testing_Global_last[!is.na(Testing_Global_last$total_tests_per_thousand),]
  Testing_Global_last <- Testing_Global_last[order(Testing_Global_last$total_tests_per_thousand, decreasing = TRUE),]
  
  
  ggObject <- ggplot(Testing_Global_last,aes(x = total_tests_per_thousand / 10, y = mortality, label=iso_code)) +
    geom_text(size = 3) +
    labs(
      title = paste("Case Mortality vs. Testing as of", today),
      y = "% Case Mortality",
      x = "% Tested"
      )

  nextSlide(ggObject, "Case Mortality vs. Testing")

  print(pptx, target = pptxfileName)
  pbPost(
    #  devices = "Phone",
    channel = "",
    type = "note", 
    title = "Message from R", 
    body = "The report is complete.",
    apikey = "o.Jb1UN5cEOOZnaZ7Tp3rsxf4vShe82xXy"
  )
  
 
  now <- as.POSIXct(Sys.time())   
  start.run <- as.POSIXct(paste(Sys.Date()+1, "04:00:00"))
  wait <- difftime(start.run, now, units = "secs")
  #Sys.sleep(wait)
  
}