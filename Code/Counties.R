# County Summary
newSection("US Counties")

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
plotPred(County = "Clark County", State = "WA", Title = "Clark County, Washington")
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
plotPred(County = "San Diego County", State = "CA", Title = "San Diego County")
plotPred(County = "Riverside County", State = "CA", Title = "Riverside County")
plotPred(County = "Imperial County", State = "CA", Title = "Imperial County")
plotPred(County = "Multnomah County", Title = "Multnomah County (Portland)")
plotPred(County = "Utah County", Title = "Utah County")
plotPred(County = "Summit County", State = "UT", Title = "Summit County, Utah")
plotPred(County = "De Soto Parish", Title = "De Soto Parish, Louisiana")
plotPred(County = "St. Francois County", Title = "St. Francois County, Missouri")
plotPred(County = "Dallas County", State = "TX", Title = "Dallas, Texas")
plotPred(County = "Collin County", State = "TX", Title = "Collin Texas")
plotPred(County = "Harris County", State = "TX", Title = "Harris County (Houston)")
plotPred(County = "Tulsa County", State = "OK", Title = "Tulsa County, Oklahoma")
plotPred(County = "McLean County", State = "IL", Title = "McLean County, Illinois")
plotPred(County = "Cook County", State = "IL", Title = "Cook County, Illinois")
plotPred(County = "Suffolk County", State = "MA", Title = "Suffolk County (Boston)")
plotPred(County = "Polk County", State = "IA", Title = "Polk County, Iowa")
plotPred(County = "Johnson County", State = "IA", Title = "Johnson County, Iowa")
plotPred(County = "Meade County", State = "SD", Title = "Meade County, South Dakota (Sturgis)")
plotPred(County = "Erie County", State = "NY", Title = "Erie County, New York")
plotPred(County = "Oakland County", State = "MI", Title = "Oakland County, Michigan")
plotPred(County = "Washtenaw County", State = "MI", Title = "Washtenaw County, Michigan (Ann Arbor)")
plotPred(County = "Wayne County", State = "MI", Title = "Wayne County (Detroit)")
plotPred(County = "City of St. Louis", Title = "St. Louis (City)")
plotPred(County = "St. Louis County", Title = "St. Louis (County)")
plotPred(County = "Baltimore City", Title = "Baltimore (City)")
plotPred(County = "Durham County", Title = "Durham County")
plotPred(County = "Cuyahoga County", Title = "Cuyahoga County (Cleveland)")
plotPred(County = "Miami-Dade County", Title = "Miami-Dade")
plotPred(County = "Maricopa County", Title = "Maricopa County")
plotPred(County = "Denver County", State = "CO", Title = "Denver County")
plotPred(County = "Arapahoe County", State = "CO", Title = "Arapahoe County")
plotPred(County = "Montrose County", State = "CO", Title = "Montrose County")
print(pptx, target = pptxfileName)
shell.exec(pptxfileName)
