# plotPred function - the main workhorse function for the analysis
plotGrowth <- function(
  results,
  Location,
  Title
)
{
  CASES <- results$CASES
  DEATHS <- results$DEATHS
  dayThreshold <- DEATHS$Date[which(DEATHS$Actual >= deathThreshold)[1]]
  
  halfWindow <- (smoothingInterval-1)/2
  start <- halfWindow + 1
  end <- which(CASES$Date == yesterday - halfWindow)
  CASES$Rate <- NA
  DEATHS$Rate <- NA
  
  X <- 1:smoothingInterval
  for (i in start:end)
  {
    Y <- CASES$Delta_Smoothed_2[(i-halfWindow):(i+halfWindow)]
    fit  <- lm(Y ~ X)
    CASES$Rate[i]  <- fit$coefficients[2] / mean(Y) * 100 
    Y <- DEATHS$Delta_Smoothed_2[(i-halfWindow):(i+halfWindow)]
    fit  <- lm(Y ~ X)
    DEATHS$Rate[i]  <- fit$coefficients[2] / mean(Y) * 100 
  }
  
  DATA <- data.frame(
    Date = c(CASES$Date[start:end],DEATHS$Date[start:end]),
    Rate = c(CASES$Rate[start:end],DEATHS$Rate[start:end]),
    Actual = c(CASES$Actual[start:end],DEATHS$Actual[start:end]),
    Location = Location,
    Cases.v.Deaths = rep(c("Cases","Deaths"), each = end-halfWindow)
  )
  DATA$Rate[is.nan(DATA$Rate)] <- NA
  DATA$Day <- difftime(DATA$Date, dayThreshold, units = "days")
  
  if (eachPlot)
  {
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
        y = "Acceleration (% growth per day)",
        title = Title
      ) +
      scale_x_date(
        date_breaks = "14 days",
        date_labels = "%b %d",
        minor_breaks = NULL
      ) +
      annotate(
        "rect", 
        xmin = dayThreshold, 
        xmax = dayThreshold+shadedDays, 
        ymin = min(DATA$Rate, na.rm=TRUE),
        ymax = max(DATA$Rate, na.rm=TRUE),
        color = "blue",
        fill = "blue",
        alpha = 0.1,
        size = 0.5
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x=element_text(angle=60, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
      )
    
    nextSlide(ggObject, Title)
  }
  return(DATA[DATA$Day >= 0,])
}


plotGrowthSummary <- function(
  DATA,
  SUBSET,
  LOCATION,
  COLOR
)
{
  DATA <- DATA[DATA$Cases.v.Deaths == SUBSET,]
  DATA <- DATA[DATA$Day <= dayCutoff,]

  CI <- data.frame(
    Day = 0:dayCutoff,
    Mean = 0,
    Lower = 0,
    Upper = 0
  ) 
  
  for (i in 1:nrow(CI))
  {
    Day <- CI$Day[i]
    X <- DATA$Rate[DATA$Day == Day]
    N <- length(X)
    mean <- mean(X)
    SD <- sd(X)
    CI$Mean[i] <- mean
    CI$Upper[i] <- mean + SD * qt(0.975,N-1) / sqrt(N)
    CI$Lower[i] <- mean - SD * qt(0.975,N-1) / sqrt(N)
  }
  
  GOM <- data.frame(
    Day = CI$Day,
    Rate = 2,
    Total = 25
  )
  
  for (i in 2:nrow(GOM))
  {
    Accel = min((1 + CI$Mean/100))
    GOM$Rate[i] = GOM$Rate[i-1] * (1 + Accel/100)
    GOM$Total[i] = GOM$Total[i-1] + GOM$Rate[i]
  }
  
  Y <- log(GOM$Total)
  fitGom <- Gompertz_fit(Y, 10000, 1)
  print(fitGom)
  GOM$Pred <- exp(Gompertz_fn(fitGom, nrow(GOM)))
  
  ggObject <- ggplot(GOM, aes(x=Day, y=Total)) + 
    geom_line() + 
    geom_line(aes(y=Pred), linetype = "dotted") + 
    scale_y_log10()
  print(ggObject)
  
  ggObject <- 
    ggplot(
      DATA,
      aes(
        x=Day,
        y = Rate
      )
    ) +  
    coord_cartesian(ylim = c(-10, 10))
    labs(
      y = "Acceleration (% per day)",
      title = paste("Summary of", SUBSET, "by", LOCATION, "as of", todayText)
    ) +
    annotate(
      "rect", 
      xmin = 0, 
      xmax = 0+shadedDays, 
      ymin = min(DATA$Rate, na.rm=TRUE),
      ymax = max(DATA$Rate, na.rm=TRUE),
      color = "blue",
      fill = "blue",
      alpha = 0.1,
      size = 0.5
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x=element_text(angle=60, hjust=1),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
    )
  for (Location in unique(DATA$Location))
  {
    ggObject <- ggObject +
      geom_line(
        data = DATA[DATA$Location == Location,], 
        size = 0.2, 
        alpha = 0.5,
        color = COLOR,
        show.legend = FALSE
      )
  }
  
  ggObject <- ggObject + 
    # geom_line(data = CI, aes(y=Mean), color = "brown") +
    geom_ribbon(
      data = CI,
      aes(ymin = Lower, ymax = Upper, x = Day),
      inherit.aes=FALSE,
      color = NA,
      fill = "grey",
      alpha = 0.5
      ) +
    geom_smooth(color = "black",se = FALSE, show.legend = FALSE)
  
  nextSlide(ggObject, paste(SUBSET, "by", LOCATION))
  return(ggObject)
}

growthPlotArrange <- function(
  ggObject1,
  ggObject2,
  Title
)
{
  plots <- list(ggObject1, ggObject2)
  grobs <- lapply(plots, as_grob)
  plot_widths <- lapply(grobs, function(x) {x$widths})
  # Aligning the left margins of all plots
  aligned_widths <- align_margin(plot_widths, "first")
  # Aligning the right margins of all plots as well
  aligned_widths <- align_margin(aligned_widths, "last")
  # Setting the dimensions of plots to the aligned dimensions
  for (i in seq_along(plots)) {
    grobs[[i]]$widths <- aligned_widths[[i]]
  }
  ggObject3 <- plot_grid(plotlist = grobs, ncol = 1)
  nextSlide(ggObject3, Title)
  
}

