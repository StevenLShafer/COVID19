# Summary Slides
newSection("Summary")
emailText <- ""
WORLD <- plotPred(Country = "Worldwide", Title = "Worldwide")
emailText <- textSummary(WORLD, "Worldwide")

USA <- plotPred(Country = "United States of America", Title = "USA")
emailText <- textSummary(USA, "In the US")

ASIA <- plotPred(Country = c("Japan","South Korea","Vietnam","Thailand"), Title = "Japan, South Korea, Thailand, Vietnam")
emailText <- textSummary(ASIA, "In the non-authoritarian Asian ensemble (population: 328MM)")

WE <- plotPred(
  Country = c("England", "France", "Germany", "Greece",
              "Italy", "Portugal", "Spain","Netherlands",
              "Flemish Region", "Luxembourg"),
  Title = "Western Europe"
) # Population 352372272
emailText <- textSummary(WE, "In Western Europe (population: 352MM)")

# World Map of Cases
# CROWS <- match(mapWorld$geounit, Population_Global$Country)
# mapWorld$Population <- Population_Global$Population[CROWS]
# mapWorld <- mapWorld[!is.na(mapWorld$Population),]
# mapWorld <- mapWorld[mapWorld$Population >  0,]
# 
# CROWS <- match(mapWorld$geounit, Cases_Global$Country)
# mapWorld$Cases <- Cases_Global[CROWS, ncol(Cases_Global)]
# mapWorld$dailyCases <- (Cases_Global[CROWS, ncol(Cases_Global)] - Cases_Global[CROWS, ncol(Cases_Global) - 8])/7
# mapWorld$lcases <- log(mapWorld$Cases)
# 
# 
# CROWS <- match(mapWorld$geounit, Deaths_Global$Country)
# mapWorld$Deaths <- Deaths_Global[CROWS, ncol(Deaths_Global)]
# mapWorld$dailyDeaths <- (Deaths_Global[CROWS, ncol(Deaths_Global)] - Deaths_Global[CROWS, ncol(Deaths_Global) - 8])/7
# mapWorld$ldeaths <- log(mapWorld$Deaths)
# mapWorld$mortality = mapWorld$Deaths / mapWorld$Cases * 100
# # Note: many locations have the same name, because it may include islands, etc. Thus, the
# # sum of cases >>> actual cases, because the same numbers are populated in all the locations
# mapWorld$mortality_limited <- pmin(mapWorld$mortality, 15) # Limit to 15% to not distort map below
# 
# mapWorld <- mapWorld[!is.na(mapWorld$lcases),]

########################
# Fisher Plots ########
# New cases per population
X <- Population_Global
CROWS <- match(X$Country, Cases_Global$Country)
X$Cases <- Cases_Global[CROWS, ncol(Cases_Global)]
X$dailyCases <- (Cases_Global[CROWS, ncol(Cases_Global)] - Cases_Global[CROWS, ncol(Cases_Global) - 8])/7

CROWS <- match(X$Country, Deaths_Global$Country)
X$Deaths <- Deaths_Global[CROWS, ncol(Deaths_Global)]
X$dailyDeaths <- (Deaths_Global[CROWS, ncol(Deaths_Global)] - Deaths_Global[CROWS, ncol(Deaths_Global) - 8])/7
X$mortality = X$Deaths / X$Cases * 100

X <- X[X$Population > 5000000,]

#X <- X[X$Country %in% c("Sweden","Denmark","Germany","Finland","Norway","USA"),]

### CASES ###
# Total cases
X$Y <- X$Cases
internationalFisherPlot(
  X,
  "Worldwide cases",
  "Total cases to date",
  "total cases",
  5
)

# Total cases per capita 
X$Y <- X$Cases  / X$Population * 1000000
internationalFisherPlot(
  X,
  "Worldwide cases",
  "Total cases to date",
  "total cases per capita",
  6,
  OneIn = TRUE
)

# Average cases over past 7 days
X$Y <- X$dailyCases  / X$Population * 1000000
internationalFisherPlot(
  X,
  "Average new cases over past 7 days",
  "Average cases / day",
  "new cases per capita over past 7 days",
  7,
  OneIn = TRUE
)

### DEATHS
# Total Deaths
X$Y <- X$Deaths
internationalFisherPlot(
  X,
  "Worldwide deaths",
  "Total deaths to date",
  "total deaths",
  5
)

# Total Deaths per million 
X$Y <- X$Deaths  / X$Population * 1000000
internationalFisherPlot(
  X,
  "Worldwide deaths",
  "Total deaths to date",
  "total deaths per capita",
  6,
  OneIn = TRUE
)

# Average Deaths per million over past 7 days
X$Y <- X$dailyDeaths  / X$Population * 1000000
internationalFisherPlot(
  X,
  "Average daily deaths over past 7 days",
  "Average deaths / day",
  "new deaths per capita over past 7 days",
  7,
  OneIn = TRUE
)


# Testing
Testing_Global_last <- Testing_Global_temp[order(Testing_Global_temp$date, decreasing = TRUE),]
Testing_Global_last <- Testing_Global_last[!duplicated(Testing_Global_last$iso_code),]
Testing_Global_last$mortality <- Testing_Global_last$total_deaths / Testing_Global_last$total_cases * 100
Testing_Global_last <- Testing_Global_last[!is.na(Testing_Global_last$total_tests_per_thousand),]
Testing_Global_last <- Testing_Global_last[order(Testing_Global_last$total_tests_per_thousand, decreasing = TRUE),]

X <- which(Testing_Global_last$iso_code == "USA")

ggObject <- ggplot(Testing_Global_last,aes(x = total_tests_per_thousand / 10, y = mortality, label=iso_code)) +
  geom_text(size = 3, hjust = 0.5, vjust = 0.5) +
  labs(
    title = paste("Case Mortality vs. Testing as of", today),
    y = "% Case Mortality",
    x = "% Tested",
    caption = "ARE: United Arab Emirates, BHR:Bahrain, MLT: Malta, ISR: Israel, LTU: Lithuania, ISL: Iceland"
  ) +
  geom_text(data = Testing_Global_last[X,], size = 3, color = "red") +
  annotate(
    "segment", 
    x = 0, 
    xend = max(Testing_Global_last$total_tests_per_thousand) / 10,
    y = Testing_Global_last$mortality[X], 
    yend = Testing_Global_last$mortality[X],
    color = "red"
  ) +
  annotate(
    "segment", 
    x = Testing_Global_last$total_tests_per_thousand[X] / 10,
    xend = Testing_Global_last$total_tests_per_thousand[X] / 10,
    y = 0,
    yend = max(Testing_Global_last$mortality, na.rm=TRUE), 
    color = "red"
  )

nextSlide(ggObject, "Case Mortality vs. Testing")

# US mortality over time 
Cases <- as.numeric(
  Cases_Global[Cases_Global$Country== "United States of America", 2:(lastColumn_Global - 7)]
)
Deaths <- as.numeric(
  Deaths_Global[Deaths_Global$Country== "United States of America", 2:(lastColumn_Global - 7)]
)

newCases  <- Cases[2:length(Cases)] - Cases[1:(length(Cases)-1)]
newDeaths <- Deaths[2:length(Deaths)] - Deaths[1:(length(Deaths)-1)]

mortality <- as.numeric(newDeaths / newCases) * 100
L <- length(mortality)
Dates <- currentDates[1:L]
window <- 13
offset <- (window - 1)/2
level <- paste(window, "day rolling median")
Z <- rollmedian (mortality, window)

D <- data.frame(
  Date = c(Dates, Dates[(offset + 1):(L-offset)], currentDates[1:length(Cases)]),
  Mortality = c(mortality, Z, Deaths / Cases * 100),
  Type = level
)
D$Type[1:length(Dates)] <- "Daily Raw"
D$Type[(nrow(D) - length(Cases) + 1):nrow(D)] <- "Cumulative"
D <- D[D$Date > as.Date("2020-02-28"),]

D$Type <- factor(D$Type, levels = c("Daily Raw", level, "Cumulative" ), ordered = TRUE)

ggObject <- ggplot(
  D,
  aes(x = Date, y = Mortality, color = Type)
) +
  geom_line(
    data = D[D$Type == "Cumulative", ],
    size = 1.5
  ) +
  geom_line(
    data = D[D$Type == level, ],
    size = 2
  ) +
  geom_point(
    data = D[D$Type == "Daily Raw", ],
    size = 1,
    shape = 3
  ) +
  coord_cartesian(
    ylim = c(0.00, 14)
  ) +
  scale_y_continuous(
    breaks = c(0,2,4,6,8,10,12,14)
  ) +
  scale_x_date(
    date_breaks = "14 days",
    date_labels = "%b %d"
  ) +
  scale_color_manual(
    values = c("green","red","black")
  ) +
  labs(
    y = "US Case Rate Mortality",
    title = "Daily vs. Cumulative US Case Rate Mortality",
    caption = "Last week excluded because deaths are often backdated",
    color = "Mortality Type:"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=60, hjust=1),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, "Mortality Trends")
##################################################################
# State Summary                                                  #
##################################################################

States$Mortality <- States$deltaCases <- States$deltaDeaths <- 
  States$dailyCases <- States$totalCases <- States$dailyDeaths <- States$totalDeaths <- 
  States$PositiveTestSlope <- States$Hospitalizations <- States$HospitalizationSlope <- 0
Cases_Long <- as.data.frame(matrix(ncol=4, nrow = 0))
names(Cases_Long) <- c("State","Date","Cases", "Deaths")

for (i in 1:nrow(States))
{
  results <- calcStats(State = States$Abbreviation[i])
  if (!is.null(results))
  {
    States$deltaCases[i] <- results$slopeCases
    States$deltaDeaths[i] <- results$slopeDeaths
    States$Mortality[i] <- results$mortality
    States$Population[i] <- results$Population
    
    States$dailyCases[i] <- mean(results$CASES$Delta[results$CASES$Date < today & results$CASES$Date >= today-8 ])
    States$totalCases[i] <- results$CASES$Actual[results$CASES$Date == yesterday]
    
    States$dailyDeaths[i] <- mean(results$DEATHS$Delta[results$DEATHS$Date < today & results$DEATHS$Date >= today-8 ])
    States$totalDeaths[i] <- results$DEATHS$Actual[results$DEATHS$Date == yesterday]
    
    C <- data.frame(
      State = States$Abbreviation[i],
      Date = currentDates, #as.Date(results$CASES$Date[results$CASES$Date < today]),
      Cases = results$CASES$Delta_Smoothed_2[results$CASES$Date < today] / max(results$CASES$Delta_Smoothed_2[results$CASES$Date < today]) * 100,
      Deaths = results$DEATHS$Delta_Smoothed_2[results$DEATHS$Date < today] / max(results$DEATHS$Delta_Smoothed_2[results$DEATHS$Date < today]) * 100
    )
    Cases_Long <- rbind(Cases_Long,C)
    
    # Positive Test Slope
    L <- ncol(Positive_USA)
    D <- data.frame(
      X = 1:14,
      Y = unlist(Positive_USA[Positive_USA$Abbreviation == States$Abbreviation[i], (L-13):L])
    )
    States$PositiveTestSlope[i] <- lm(Y~X, data = D)$coefficients[2] / mean(D$Y) * 100

    # Hospitalizations
    L <- ncol(Hospitalization_USA_zeroOne)
    States$Hospitalizations[i] <- Hospitalization_USA_zeroOne$Hospitalizations[
      Hospitalization_USA_zeroOne$Abbreviation == States$Abbreviation[i] & 
        Hospitalization_USA_zeroOne$Date == yesterday]
    
    # Hospitalization Slope
    L <- ncol(Hospitalization_USA_zeroOne)
    D <- data.frame(
      X = 1:14,
      Y = Hospitalization_USA_zeroOne$Hospitalizations[
        Hospitalization_USA_zeroOne$Abbreviation == States$Abbreviation[i] & 
        Hospitalization_USA_zeroOne$Date >= today - 14
      ]
    )
    States$HospitalizationSlope[i] <- lm(Y~X, data = D)$coefficients[2] / mean(D$Y) * 100
  }
}
States$state <- States$State   # Required for plot_usmap().... whatever

# Add testing
CROWS <- match(States$Abbreviation,Testing_USA$Abbreviation)
States$Tests <- Testing_USA[CROWS, ncol(Testing_USA)]
States$TestingFraction <- States$Tests / States$Population * 100

# Add Masks
MASKS <- read.xlsx(paste0(dirSheets,"masks.xlsx")) # As of July 20
CROWS <- match(States$State, MASKS$State)
States$Masks <- MASKS$Statewide.Requirement[CROWS]


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
stateColors <- c("red", "pink", "white","lightgreen", "green")
ggObject <- plot_usmap(data = States, values = "signCase", color = "black") +
  scale_fill_manual(values = stateColors, name = "New cases are:", drop=FALSE) +
  theme(legend.position = "right") +
  labs(
    title = paste("New cases by state as of", Sys.Date())
  )
nextSlide(ggObject, "Change in New Cases per Day")

ggObject <- ggplot(Cases_Long[Cases_Long$Date >= today - 58 & Cases_Long$Date < today,], aes(Date, Cases)) +
  geom_line() +
  scale_x_date(
    breaks = c(today - 0:4 * 14)-1,
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = c(0,25,50,75, 100)
  ) +
  coord_cartesian(ylim = c(0,100)) +
  facet_geo(~ State, grid = "us_state_grid4") +
  labs(
    y = "Percent of Peak",
    title = "Daily Cases as a Percent of Peak Cases") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=70, hjust=1, size=7),
    axis.text.y=element_text(size=6),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, "Cases as a Percent of Peak Cases")

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
  scale_fill_manual(values = stateColors, name = "New deaths are:", drop=FALSE) +
  theme(legend.position = "right") +
  labs(
    title = paste("New deaths by state as of", Sys.Date())
  )
nextSlide(ggObject, "Change in New Deaths per Day")

ggObject <- ggplot(Cases_Long[Cases_Long$Date >= today - 58 & Cases_Long$Date < today,], aes(Date, Deaths)) +
  geom_line() +
  scale_x_date(
    breaks = c(today - 0:4 * 14)-1,
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = c(0,25,50,75, 100)
  ) +
  coord_cartesian(ylim = c(0,100)) +
  facet_geo(~ State, grid = "us_state_grid4") +
  labs(
    y = "Percent of Peak",
    title = "Daily Deaths as a Percent of Peak Deaths") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=70, hjust=1, size=7),
    axis.text.y=element_text(size=6),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, "Deaths as a Percent of Peak Deaths")

States$Quadrant <- 1
States$Quadrant[States$deltaCases < 0 & States$deltaDeaths < 0] <- 2
States$Quadrant[States$deltaCases > 0 & States$deltaDeaths < 0] <- 3
States$Quadrant[States$deltaCases < 0 & States$deltaDeaths > 0] <- 4

ggObject <- ggplot(States,aes(x = deltaCases, y = deltaDeaths, label=Abbreviation)) +
  geom_text(size = 3, aes(color = as.factor(Quadrant)), show.legend=FALSE) +
  labs(
    title = paste("Change in cases vs change in deaths over last 14 days", today),
    y = "Change in deaths (%/day)",
    x = "Change in cases (%/day)"
  ) +
  coord_cartesian() +
  scale_color_manual(values = c("red","forestgreen","blue","magenta")) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = min(States$deltaDeaths),
    yend = max(States$deltaDeaths),
    color = "black"
  ) +
  annotate(
    "segment",
    y = 0,
    yend = 0,
    x = min(States$deltaCases),
    xend = max(States$deltaCases),
    color = "black"
  )

nextSlide(ggObject, "Change in cases vs change in deaths")


########################

# Fisher Plots for States 
# Total Cases per population
States$Y <- States$totalCases
stateFisherPlot(
  States,
  "Total US COVID-19 Cases",
  "Total cases",
  "cases",
  5
)

# Total Cases per population
States$Y <- States$totalCases  / States$Population * 1000000
stateFisherPlot(
  States,
  "Total US COVID-19 Cases",
  "Total Cases",
  "total cases per capita",
  5,
  OneIn = TRUE
)

States$Y <- States$dailyCases  / States$Population * 1000000
stateFisherPlot(
  States,
  "Average US COVID-19 cases over the past 7 days",
  "New Cases / Day",
  "cases per day per capita over the past 7 days",
  6,
  OneIn = TRUE
)

States$Y <- States$totalDeaths
stateFisherPlot(
  States,
  "Total US COVID-19 Deaths",
  "Total Deaths",
  "deaths",
  5
)

States$Y <- States$totalDeaths  / States$Population * 1000000
stateFisherPlot(
  States,
  "Total US COVID-19 Deaths",
  "Total Deaths",
  "deaths per capita",
  5,
  OneIn = TRUE
)

States$Y <- States$dailyDeaths  / States$Population * 1000000
stateFisherPlot(
  States,
  "Average US COVID-19 deaths over the past 7 days",
  "Deaths / Day",
  "deaths per day per capita over the past 7 days",
  6,
  OneIn = TRUE
)

States$Y <- States$TestingFraction
stateFisherPlot(
  States,
  "Percent Tested",
  "Percent Tested",
  "percent tested",
  6
)

# Positive Tests
States$Y <- Positive_USA[match(States$Abbreviation, Positive_USA$Abbreviation),ncol(Positive_USA)]
stateFisherPlot(
  States,
  "Percent of Positive COVID Tests",
  "Percent of Positive Tests",
  "percent positive tests",
  6
)

ggObject <- ggplot(Positive_USA_zeroOne[Positive_USA_zeroOne$Date >= "2020-03-01",], aes(Date, Positive)) +
  geom_line() +
  scale_x_date(
    date_breaks = "28 days",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = c(0,1),
    labels = c("min","max")
  ) +
  coord_cartesian(ylim = c(0,1)) +
  facet_geo(~ Abbreviation, grid = "us_state_grid4") +
  labs(
    y = "Fraction positive from min to max",
    title = "Positive fraction trends from min to max") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=70, hjust=1, size=7),
    axis.text.y=element_text(size=6),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, "Positive fraction trends")

States$Y <- States$PositiveTestSlope
stateFisherPlot(
  States,
  "Change in positive tests over past 14 days",
  "Change in positive tests (%/day)",
  "daily change in positive tests",
  6
)


# Hospitalizatons
States$Y <- States$Hospitalizations * 100
stateFisherPlot(
  States,
  "Current hospitalizations as a percent of peak since February",
  "Hospitalizations (% of peak)",
  "current hospitalizations as a percent of peak rate",
  6
)

ggObject <- ggplot(Hospitalization_USA_zeroOne[Hospitalization_USA_zeroOne$Date >= "2020-03-18",], aes(Date, Hospitalizations)) +
  geom_line() +
  scale_x_date(
    date_breaks = "28 days",
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = c(0,1),
    labels = c("min","max")
  ) +
  coord_cartesian(ylim = c(0,1)) +
  facet_geo(~ Abbreviation, grid = "us_state_grid4") +
  labs(
    y = "Hospitalizations from min to max",
    title = "Hospitalizations trends from min to max") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=70, hjust=1, size=7),
    axis.text.y=element_text(size=6),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, "Hospitalizations trends")

# Hospitalizatons
States$Y <- States$HospitalizationSlope
stateFisherPlot(
  States,
  "Change in hospitalizations over past 14 days",
  "Change in hospitalizations (%/day)",
  "daily change in hospitalizations",
  6
)

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
  coord_cartesian()

nextSlide(ggObject, "Case Mortality vs. Testing")
print(pptx, target = pptxfileName)
shell.exec(pptxfileName)

writeLines(emailText,paste0(dirTodayUpdate,"EmailText.",timestamp,".txt"))
