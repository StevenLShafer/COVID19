# plotPred function - the main workhorse function for the analysis
plotGrowth <- function(
  results,
  Title
)
{

  DATA              <- results$DATA
  CASES             <- results$CASES
  DEATHS            <- results$DEATHS
  mortality         <- results$mortality
  slopeCases        <- results$slopeCases
  slopeDeaths       <- results$slopeDeaths
  Population        <- results$Population
  predictedCases    <- results$predictedCases
  predictedDeaths   <- results$predictedDeaths
  fitGom            <- results$fitGom
  linfitCases       <- results$linfitCases
  linfitDeaths      <- results$linfitDeaths
  todayIndex        <- results$todayIndex
  yesterdayCases    <- results$yesterdayCases
  yesterdayDeaths   <- results$yesterdayDeaths
  nRow              <- results$nRow
  
  day25 <- DATA$Date[which(DEATHS$Actual >= 25)[1]]
  
  N <- which(CASES$Date == today - 8)
  CASES$Rate <- NA
  DEATHS$Rate <- NA
  X <- 1:15
  for (i in 8:N)
  {
    Y <- CASES$Delta_Smoothed_2[(i-7):(i+7)]
    fit  <- lm(Y ~ X)
    CASES$Rate[i]  <- fit$coefficients[2] / mean(Y) * 100 
    Y <- DEATHS$Delta_Smoothed_2[(i-7):(i+7)]
    fit  <- lm(Y ~ X)
    DEATHS$Rate[i]  <- fit$coefficients[2] / mean(Y) * 100 
  }
  
  DATA <- data.frame(
    Date = c(CASES$Date[8:N],DEATHS$Date[8:N]),
    Rate = c(CASES$Rate[8:N],DEATHS$Rate[8:N]),
    Actual = c(CASES$Actual[8:N],DEATHS$Actual[8:N]),
    Location = Title,
    Cases.v.Deaths = rep(c("Cases","Deaths"),each = N-7)
  )
  DATA$Rate[is.nan(DATA$Rate)] <- NA
  DATA$Days <- difftime(DATA$Date, day25, units = "days")
    
  ggObject <-
    ggplot(
      DATA,
      aes(
        x=Date,
        y = Rate,
        color = Cases.v.Deaths
        )
    ) +  
    geom_line() +
    labs(
      y = "Rate (% per day)",
      title = Title
    ) +
    scale_x_date(
      date_breaks = "7 days",
      date_labels = "%b %d",
      minor_breaks = NULL
    ) +
    annotate(
      "rect", 
      xmin = day25, 
      xmax = day25+30, 
      ymin = min(DATA$Rate, na.rm=TRUE),
      ymax = max(DATA$Rate, na.rm=TRUE),
      color = "blue",
      fill = "blue",
      alpha = 0.1,
      size = 0.5
    ) +
    annotate(
      "segment", 
      x = day25 + 30, 
      xend = day25 + 30, 
      y = min(DATA$Rate, na.rm=TRUE),
      yend = max(DATA$Rate, na.rm=TRUE),
      color = "blue",
      size = 0.5
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x=element_text(angle=60, hjust=1),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
    )
  
  nextSlide(ggObject, Title)
  return(DATA)
}
