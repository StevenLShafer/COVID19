# Summary Slides
newSection("Summary")

emailText <- paste(
  readLines(paste0(dirCode,"email.body.start.txt")),
  collapse = "\n"
)

WORLD <- plotPred(Country = "Worldwide", Title = "Worldwide")
emailText <- textSummary(WORLD, "Worldwide")

USA <- plotPred(Country = "United States of America", Title = "USA")
emailText <- textSummary(USA, "In the US")

# X <- USA$CASES[USA$CASES$Date > as.Date("2020-03-02") & USA$CASES$Date < today ,]
# breaks = as.Date(as.Date("2020-03-02") + 0:24*7)
# labels = format(breaks, format = "%a, %b-%d")
# ggplot(X,aes(x = Date, y = Delta)) +
#   geom_col(width=1.0) +
#   scale_x_date(
#     breaks = breaks,
#     labels = labels,
#     minor_breaks = NULL,
#     expand = c(0,0)
# #    limits = c(as.Date("2020-03-09"), as.Date("2020-08-16"))
#   ) +
#   labs(
#     title = "Daily US cases since March 2020",
#     y = "Daily COVID-19 Cases"
#   ) +
#   scale_y_continuous(label = comma) +
#   theme(
#     axis.text.x=element_text(angle=60, hjust=1)
#   ) +
#   coord_cartesian(xlim = c(as.Date("2020-03-02"), as.Date("2020-08-17")))
# 
# 
# X <- USA$DEATHS[USA$DEATHS$Date > as.Date("2020-03-02") & USA$DEATHS$Date < today ,]
# ggplot(X,aes(x = Date, y = Delta)) +
#   geom_col(width=1.0) +
#   scale_x_date(
#     breaks = breaks,
#     labels = labels,
#     minor_breaks = NULL,
#     expand = c(0,0)
#     #    limits = c(as.Date("2020-03-09"), as.Date("2020-08-16"))
#   ) +
#   labs(
#     title = "Daily US cases since March 2020",
#     y = "Daily COVID-19 Deaths"
#   ) +
#   scale_y_continuous(label = comma) +
#   theme(
#     axis.text.x=element_text(angle=60, hjust=1)
#   ) +
#   coord_cartesian(xlim = c(as.Date("2020-03-02"), as.Date("2020-08-17")))

ASIA <- plotPred(
  Country = c("Japan","South Korea","Vietnam","Thailand"), 
  Title = "Non-authoritarian Asian ensemble", 
  Subtitle = "Japan, South Korea, Thailand, and Vietnam (Population = 328 MM)"
) 

emailText <- textSummary(ASIA, "In the non-authoritarian Asian ensemble (population: 328MM)")

WE <- plotPred(
  Country = c("United Kingdom", "France", "Germany", "Greece",
              "Italy", "Portugal", "Spain","Netherlands",
              "Flemish Region", "Luxembourg"),
  Title = "Western Europe",
  Subtitle = "Belgium, France, Germany, Greece, Italy, Portugal, Spain, Netherlands, Luxembourg, and the UK (Population = 344MM)"
) # Population 343970478
emailText <- textSummary(WE, "In Western Europe (population: 344MM)")

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


# Plot for Rob Jackler
USA$DEATHS$Location <- USA$CASES$Location <- "USA (318MM)"
WE$DEATHS$Location <- WE$CASES$Location <- "Western Europe (344MM)"
AllCases <- rbind(USA$CASES, WE$CASES)
AllCases$Type <- "Cases"
AllDeaths <- rbind(USA$DEATHS, WE$DEATHS)
AllDeaths$Type <- "Deaths"
JacklerData <- rbind(AllCases, AllDeaths)
JacklerData <- JacklerData[JacklerData$Date > as.Date("2020-02-29") & JacklerData$Date <= today,]

JacklerData <- JacklerData[JacklerData$Date > as.Date("2020-02-29") & JacklerData$Date <= today,]

labels <- data.frame(
  Last = round(
    JacklerData$Delta[JacklerData$Date == yesterday],
    0),
  Date = today + 1,
  Type = c("Cases","Cases","Deaths","Deaths"),
  Location = c(
    "USA (318MM)", 
    "Western Europe (344MM)",
    "USA (318MM)", 
    "Western Europe (344MM)"
  )
)
labels$Label <- prettyNum(labels$Last, big.mark = ",", scientific = FALSE)
SignCases <- sign(labels$Last[1]-labels$Last[2])
SignDeaths <- sign(labels$Last[3]-labels$Last[4])
distance = 0.5
labels$Y <- c(
  labels$Last[1] * exp(SignCases * distance),
  labels$Last[2] * exp(-SignCases * distance),
  labels$Last[3] * exp(SignDeaths * distance),
  labels$Last[4] * exp(-SignDeaths * distance)
)

ggObject <- ggplot(JacklerData, aes(x = Date, y = Delta_Smoothed_2, color = Location, linetype = Type)) +
  geom_line(size = 1.2) +
  scale_x_date(
    date_breaks = "14 days",
    date_labels = "%b %d",
    minor_breaks = NULL
  ) +
  scale_y_log10(
    breaks = c(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000),
    labels = c("3","10", "30","100","300","1,000","3,000", "10,000", "30,000","100,000")
    ) +
  scale_color_manual(values = c("dodgerblue","coral")) +
  labs(
    y = "Daily Cases",
    title = "Comparison of COVID-19 Cases & Deaths between US & Europe",
    subtitle = "Log plot of 7 day average",
    caption = "The numbers on the right are yesterday's figures, and will differ a bit from the plotted rolling mean"
  ) +
  annotation_logticks() +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    plot.subtitle = element_text(size = 8)
  ) +
  geom_text(
    data = labels, 
    aes(
      y = Y,
      label = Label
    ),
    show.legend = FALSE
  )
nextSlide(ggObject, "Daily Cases in the USA and Western Europe")

emailText <- paste(
  emailText,
  email.list.start,
  "The relative numbers of cases and deaths in the USA and Western Europe shows xxxx:",
  add_ggplot(
    plot_object = ggObject,
    width = 9,
    height = 4.95,
    alt = NULL,
    align = "left",
    float = "none"
  ),
  email.list.end
)


########################
# Fisher Plots ########
# New cases per population
Countries <- Population_Global
CROWS <- match(Countries$Country, Cases_Global$Country)
N <- ncol(Cases_Global)
Countries$Cases <- Cases_Global[CROWS, N]
Countries$dailyCases <- (Cases_Global[CROWS, N] - Cases_Global[CROWS, N - 8])/7
for (c in CROWS)
{
Countries$CVCases[c] <- sd(unlist(Cases_Global[c, (N-28):N])) / mean(unlist(Cases_Global[c, (N-28):N]))
}

CROWS <- match(Countries$Country, Deaths_Global$Country)
N <- ncol(Deaths_Global)
Countries$Deaths <- Deaths_Global[CROWS, N]
Countries$dailyDeaths <- (Deaths_Global[CROWS, N] - Deaths_Global[CROWS, N - 8])/7
Countries$mortality = Countries$Deaths / Countries$Cases * 100
Countries$percentCases <- Countries$Cases / Countries$Population * 100

for (c in CROWS)
{
  Countries$CVDeaths[c] <- sd(unlist(Deaths_Global[c, (N-28):N])) / mean(unlist(Deaths_Global[c, (N-28):N]))
}
Countries <- Countries[Countries$Population > 5000000,]

#Countries <- Countries[Countries$Country %in% c("Sweden","Denmark","Germany","Finland","Norway","USA"),]

### CASES ###
# Total cases
Countries$Y <- Countries$Cases
internationalFisherPlot(
  Countries,
  "Worldwide cases",
  "Total cases to date",
  "total cases",
  5
)

# Total cases per capita 
Countries$Y <- Countries$Cases  / Countries$Population * 1000000
internationalFisherPlot(
  Countries,
  "Worldwide cases",
  "Total cases to date",
  "total cases per capita",
  6,
  OneIn = TRUE
)

# Average cases over past 7 days
Countries$Y <- Countries$dailyCases  / Countries$Population * 1000000
internationalFisherPlot(
  Countries,
  "Average new cases over past 7 days",
  "Average cases / day",
  "new cases per capita over past 7 days",
  7,
  OneIn = TRUE
)

### DEATHS
# Total Deaths
Countries$Y <- Countries$Deaths
internationalFisherPlot(
  Countries,
  "Worldwide deaths",
  "Total deaths to date",
  "total deaths",
  5
)

# Total Deaths per million 
Countries$Y <- Countries$Deaths  / Countries$Population * 1000000
internationalFisherPlot(
  Countries,
  "Worldwide deaths",
  "Total deaths to date",
  "total deaths per capita",
  6,
  OneIn = TRUE
)

# Average Deaths per million over past 7 days
Countries$Y <- Countries$dailyDeaths  / Countries$Population * 1000000
internationalFisherPlot(
  Countries,
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

if (weekDay == 1) # Monday only
{
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
}

# CV for international data
if (weekDay == 0) # Sunday only
{
  X <- Countries[!is.nan(Countries$CVCases) & !is.nan(Countries$CVDeaths),]
  X <- X[X$CVDeaths > 0, ]
  X <- X[order(X$CVDeaths, decreasing = FALSE),]
  ggObject <- ggplot(X,aes(x = CVCases, y = CVDeaths, label=Abbreviation)) +
    geom_text(size = 3, hjust = 0.5, vjust = 0.5) +
    labs(
      title = paste("Coefficient of variation for cases and deaths as of", today),
      x = "Cases CV",
      y = "Deaths CV",
      caption = "CV calculated over last 28 days"
    ) +
    annotation_logticks() +
    scale_x_log10(breaks = c(0.01, 0.01, 0.1, 1)) +
    scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1)) +
    coord_cartesian(xlim = c(0.01, 1), ylim = c(0.001,1))
  nextSlide(ggObject, "CV for Cases and Deaths")
}

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

#################################################################################################################
# State Summary                                                                                                 #
#################################################################################################################

States$Mortality <- 
  States$slopeCases <- States$slopeDeaths <- 
  States$dailyCases <- States$totalCases <- 
  States$dailyDeaths <- States$totalDeaths <- 
  States$slopeTests <- States$slopePositiveTests <- 
  States$Hospitalizations <- States$slopeHospitalizations <- 
  States$CVCases <- States$CVDeaths <- 0
Cases_Long <- as.data.frame(matrix(ncol=4, nrow = 0))
names(Cases_Long) <- c("State","Date","Cases", "Deaths")

for (i in 1:nrow(States))
{
  results <- calcStats(State = States$Abbreviation[i])
  if (!is.null(results))
  {
    States$slopeCases[i] <- results$slopeCases
    States$slopeDeaths[i] <- results$slopeDeaths
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
    
    # Testing Slope
    D <- data.frame(
      X = 1:14,
      Y = Testing_USA_zeroOne$Testing[Testing_USA_zeroOne$Abbreviation == States$Abbreviation[i] & Testing_USA_zeroOne$Date >= today-14]
    )
    States$slopeTests[i] <- lm(Y~X, data = D)$coefficients[2] / mean(D$Y) * 100
    
    # Positive Test Slope
    L <- ncol(Positive_USA)
    D <- data.frame(
      X = 1:14,
      Y = unlist(Positive_USA[Positive_USA$Abbreviation == States$Abbreviation[i], (L-13):L])
    )
    States$slopePositiveTests[i] <- lm(Y~X, data = D)$coefficients[2] / mean(D$Y) * 100

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
    States$slopeHospitalizations[i] <- lm(Y~X, data = D)$coefficients[2] / mean(D$Y) * 100
    
   # Coefficient of Variation
    N <- nrow(C)
    States$CVCases[i] <- sd(C$Cases[(N-28):N]) / mean(C$Cases[(N-28):N])
    States$CVDeaths[i] <- sd(C$Deaths[(N-28):N]) / mean(C$Deaths[(N-28):N])
  }
}
States$state <- States$State   # Required for plot_usmap().... whatever

# Impose limit of -5% on decrease to compensate for reporting problems
States$slopeTests[States$slopeTests < -5] <- -5


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
States$signCase[States$slopeCases < -3] <- "Decreasing > -3%"
States$signCase[States$slopeCases >= -3 & States$slopeCases < -1] <- "Decreasing between -1% and -3%"
States$signCase[States$slopeCases <= 3 & States$slopeCases > 1] <- "Increasing between +1% and +3%"
States$signCase[States$slopeCases > 3] <- "Increasing > +3%"
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

emailText <- paste(
    emailText,
    email.list.start,
    "The red/green map for new cases shows:",
    add_ggplot(
      plot_object = ggObject,
      width = 9,
      height = 4.5,
      alt = NULL,
      align = "left",
      float = "none"
    ),
    email.list.end
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
States$slopeDeaths[States$slopeDeaths < -6] <- -6
States$slopeDeaths[States$slopeDeaths > 6] <- 6
States$signDeath <- "No Change (-0.1% to +0.1%)"
States$signDeath[States$slopeDeaths < -0.5] <- "Decreasing > -0.5%"
States$signDeath[States$slopeDeaths >= -0.5 & States$slopeDeaths < -0.1] <- "Decreasing between -0.1% and -0.5%"
States$signDeath[States$slopeDeaths <= 0.5 & States$slopeDeaths > 0.1] <- "Increasing between +0.1% and +0.5%"
States$signDeath[States$slopeDeaths > 0.5] <- "Increasing > +0.5%"
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

emailText <- paste(
  emailText,
  email.list.start,
  "The red/green map for daily deaths shows:",
  add_ggplot(
    plot_object = ggObject,
    width = 9,
    height = 4.5,
    alt = NULL,
    align = "left",
    float = "none"
  ),
  email.list.end
)

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
States$Quadrant[States$slopeCases < 0 & States$slopeDeaths < 0] <- 2
States$Quadrant[States$slopeCases > 0 & States$slopeDeaths < 0] <- 3
States$Quadrant[States$slopeCases < 0 & States$slopeDeaths > 0] <- 4

ggObject <- ggplot(States,aes(x = slopeCases, y = slopeDeaths, label=Abbreviation)) +
  geom_text(size = 3, aes(color = as.factor(Quadrant)), show.legend=FALSE) +
  labs(
    title = paste("Change in cases vs change in deaths over last 14 days as of", today),
    y = "Change in deaths (%/day)",
    x = "Change in cases (%/day)"
  ) +
  coord_cartesian() +
  scale_color_manual(values = c("red","forestgreen","blue","magenta")) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = min(States$slopeDeaths),
    yend = max(States$slopeDeaths),
    color = "black"
  ) +
  annotate(
    "segment",
    y = 0,
    yend = 0,
    x = min(States$slopeCases),
    xend = max(States$slopeCases),
    color = "black"
  )

nextSlide(ggObject, "Change in cases vs change in deaths")

emailText <- paste(
  emailText,
  email.list.start,
  "The four quadrant map for changes in cases vs changes in deaths:",
  add_ggplot(
    plot_object = ggObject,
    width = 9,
    height = 4.5,
    alt = NULL,
    align = "left",
    float = "none"
  ),
  email.list.end
)


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

# Testing Trends
ggObject <- ggplot(Testing_USA_zeroOne[Testing_USA_zeroOne$Date >= "2020-03-01",], aes(Date, Testing)) +
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
    y = "Daily testing from min to max",
    title = "Daily testing trends from min to max",
    caption = "Line = Friedman's supersmoother"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=70, hjust=1, size=7),
    axis.text.y=element_text(size=6),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, "Daily testing trends")

States$Y <- States$slopeTests
stateFisherPlot(
  States,
  "Change in daily tests over past 14 days",
  "Change in daily tests (%/day)",
  "daily increase in tests",
  6
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

States$Y <- States$slopePositiveTests
stateFisherPlot(
  States,
  "Change in positive tests over past 14 days",
  "Change in positive tests (%/day)",
  "daily change in positive tests",
  6
)


# Four quadrant graph for testing and positivity

States$Quadrant <- 1
States$Quadrant[States$slopeTests < 0 & States$slopePositiveTests < 0] <- 2
States$Quadrant[States$slopeTests > 0 & States$slopePositiveTests < 0] <- 3
States$Quadrant[States$slopeTests < 0 & States$slopePositiveTests > 0] <- 4

ggObject <- ggplot(States,aes(x = slopeTests, y = slopePositiveTests, label=Abbreviation, size = dailyDeaths)) +
  geom_text(aes(color = as.factor(Quadrant)), show.legend=FALSE) +
  labs(
    title = paste("Change in tests vs change in positive tests last 14 days as of", today),
    y = "Change in positive tests (%/day)",
    x = "Change in tests (%/day)",
    caption = "Size of the state font reflects the number of deaths over the past 7 days."
  ) +
  coord_cartesian() +
  scale_color_manual(values = c("magenta","blue","forestgreen","red")) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = min(States$slopePositiveTests),
    yend = max(States$slopePositiveTests),
    color = "black"
  ) +
  annotate(
    "segment",
    y = 0,
    yend = 0,
    x = min(States$slopeTests, -1),
    xend = max(States$slopeTests),
    color = "black"
  )
nextSlide(ggObject, "Change in tests vs change in positive tests")

emailText <- paste(
  emailText,
  email.list.start,
  "The four quadrant graph for change in testing vs change in positivity shows  xxxsx:",
  add_ggplot(
    plot_object = ggObject,
    width = 9,
    height = 4.5,
    alt = NULL,
    align = "left",
    float = "none"
  ),
  email.list.end
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
States$Y <- States$slopeHospitalizations
stateFisherPlot(
  States,
  "Change in hospitalizations over past 14 days",
  "Change in hospitalizations (%/day)",
  "daily change in hospitalizations",
  6
)

if (weekDay == 1) # Monday only
{
  # Testing vs. Mortality
  CROWS <- match(States$Abbreviation,Testing_USA$Abbreviation)
  States$Tests <- Testing_USA[CROWS, ncol(Testing_USA)]
  States$TestingFraction <- States$Tests / States$Population * 100
  
  ggObject <- ggplot(States,aes(x = TestingFraction, y = Mortality * 100, label=Abbreviation)) +
    geom_text(size = 3) +
    labs(
      title = paste("Mortality vs. Testing as of", today),
      y = "% Mortality",
      x = "% Tested"
    ) +
    coord_cartesian()
  nextSlide(ggObject, "Case Mortality vs. Testing")
}
# CV for state data
if (weekDay == 0) # Sunday Only
{
  X <- States[!is.nan(States$CVCases) & !is.nan(States$CVDeaths),]
  X <- X[X$CVDeaths > 0, ]
  X <- X[order(X$CVDeaths, decreasing = FALSE),]
  ggObject <- ggplot(X,aes(x = CVCases, y = CVDeaths, label=Abbreviation)) +
    geom_text(size = 3, hjust = 0.5, vjust = 0.5) +
    labs(
      title = paste("Coefficient of variation for cases and deaths as of", today),
      x = "Cases CV",
      y = "Deaths CV",
      caption = "CV calculated over last 28 days"
    ) +
    annotation_logticks() +
    scale_x_log10(breaks = c(0.01, 0.01, 0.1, 1)) +
    scale_y_log10(breaks = c(0.01, 0.01, 0.1, 1)) +
    coord_cartesian(xlim = c(0.01, 1.317), ylim = c(0.01,1.317))
  nextSlide(ggObject, "CV for Cases and Deaths")
}

################################################################################
# Counties                                                                     #
################################################################################

Counties$deltaCases <- 0
Counties$deltaDeaths <- 0
Counties$Population <- 0
Counties$Cases <- 0
Counties$Deaths <- 0
Counties$Mortality <- 0
Counties$growthCases <- 0
Counties$State <- ""
for (i in 1:nrow(Counties))
{
  x <- which(Cases_USA$CountyFIPS == Counties$FIPS[i])
  Counties$State[i] <- Cases_USA$State[x]
  results <- calcStats(County = Cases_USA$County.Name[x], State = Cases_USA$State[x])
  if (!is.null(results))
  {
    Counties$deltaCases[i]  <- results$slopeCases
    Counties$deltaDeaths[i] <- results$slopeDeaths
    Counties$Cases[i]   <- results$yesterdayCases
    Counties$Deaths[i]  <- results$yesterdayDeaths
    Counties$Population[i] <- results$Population
    Counties$Mortality[i]  <- results$mortality
    Counties$growthCases[i] <- 
      ((results$CASES$Actual[results$CASES$Date == today - 1] - results$CASES$Actual[results$CASES$Date == today - 22]) / 21) / results$Population * 100
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
################################################################################

# New cases / day
ggObject <- plot_usmap(data = Counties, values = "signCase", color = "black") +
  scale_fill_manual(values = c("red", "pink", "white","lightgreen", "green"), name = "Direction") +
  theme(legend.position = "right") +
  labs(
    title = paste("Trends by county as of", Sys.Date()),
    caption = "NA = Inadequate data"
  )
nextSlide(ggObject, "Change in New Cases per Day")

emailText <- paste(
  emailText,
  email.list.start,
  "The county level graph identifies more precisely where cases are rising:",
  add_ggplot(
    plot_object = ggObject,
    width = 9,
    height = 4.95,
    alt = NULL,
    align = "left",
    float = "none"
  ),
  email.list.end
)

if (weekDay == 1) # Monday only
{
  
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
  
  maxPercentCases <- which(subset$percentCases == max(subset$percentCases))
  x <- which(Cases_USA$CountyFIPS == subset$FIPS[maxPercentCases])
  results <- calcStats(County = Cases_USA$County.Name[x], State = Cases_USA$State[x])
  dailyRate <- round(
    (results$CASES$Actual[results$CASES$Date == today - 1] - 
    results$CASES$Actual[results$CASES$Date == today - 15]) / 14,
    0
  )
  caseRate <- 
    dailyRate /
    results$CASES$Actual[results$CASES$Date == today - 1] * 100
  emailText <- paste0(
    email.list.start,
    emailText,
    " The county with the most cases per capita is ",
    subset$Name[maxPercentCases],
    ", which has reported ",
    prettyNum(
      subset$Cases[maxPercentCases],
      big.mark = ",", scientific = FALSE),
    " cases in a population of ",
    prettyNum(
      subset$Population[maxPercentCases],
      big.mark = ",", scientific = FALSE),
    " (",
    round(subset$percentCases[maxPercentCases],1),
    "%, about 1 in every ",
    round(1/subset$percentCases[maxPercentCases] * 100,0),
    " individuals). Cases in ",
    subset$Name[maxPercentCases],
    " are rising at just ",
    dailyRate, 
    " cases per day (",
    round(caseRate,2),
    "%). "
  )
  
  fractionUSA <- 
  subset$percentCases[maxPercentCases] / 
  (USA$CASES$Actual[USA$CASES$Date == yesterday] / USA$Population * 100)
  deathsUSA <- USA$DEATHS$Actual[USA$DEATHS$Date == yesterday] * fractionUSA
  emailText <- paste0(
    emailText,
    "If this is the natural limit of documented transmission, ",
    "then the US is currently at just ",
    round(1/fractionUSA * 100, 0),
    "% of this limit. Extrapolating to 100%, the USA will eventually see ",
    prettyNum(
      round(deathsUSA, 0),
      big.mark = ",", scientific = FALSE),
    " deaths in the absence of a vaccine.",
    email.list.end
  )
}

subset <- Counties[
  Counties$Population >= 10000  & !is.na(Counties$growthCases),
]

ggplot(subset, aes(x = percentCases, y = growthCases)) +
  geom_point() +
  labs(
    title = "Percent Cases vs. Daily Growth (as % of population)",
    x = "Cases as a percent of population",
    y = "Daily growth as a percent of the population "
  )


print(pptx, target = pptxfileName)
shell.exec(pptxfileName)

emailText <- paste(
  emailText,
  paste(
    readLines(paste0(dirCode,"email.body.end.txt")),
    collapse = "\n"
  )
)
#  todaysText <- paste0(dirTodayUpdate,"EmailText.",timestamp,".html")
#  writeLines(emailText, todaysText)
#  shell.exec(todaysText)

send.mail(
  from = "stanpumpR@gmail.com",
  to = "steveshafer@gmail.com",
  subject = paste("Daily COVID Update for", format(today, "%A, %B %d, %Y")),
  body = emailText,
  html = TRUE,
  smtp = list(
    host.name = "smtp.gmail.com",
    port = 465,
    user.name = config::get("email_username"),
    passwd = config::get("email_password"),
    ssl = TRUE),
  authenticate = TRUE
)

