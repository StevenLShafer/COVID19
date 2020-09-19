# plotPred function - the main workhorse function for the analysis
plotPred <- function(
  County = NULL,
  State = NULL,
  Country = NULL,
  Title = NULL,
  Subtitle = NULL,
  modelStart = NULL,
  weight = NULL
)
{
  debug <- FALSE
  if (!exists("modelStart"))
  {
    County <- "Androscoggin County"
    State <- "ME"
    Country <- NULL
    Title <- NULL
    Subtitle <- NULL
    modelStart <- NULL
    debug <- TRUE
    weight <- NULL
  }
  if (is.null(Title)) 
  {
    if (!is.null(County))
    {
      Title <- County
      if (!is.null(State)) Title <- paste0(Title, ", ", States$State[States$Abbreviation == State])
    } else {
      if (!is.null(State))
      {
        Title <- States$State[States$Abbreviation == State] 
      } else {
        Title <- Country 
      }
    }
  }
  if (is.null(modelStart)) modelStart <- today - daysGompertzFit
  if (is.null(weight)) weight <- 0
  
  results <- calcStats(
    County = County,
    State = State,
    Country = Country,
    modelStart = modelStart,
    weight = weight
  )
  
  if (is.null(results))
  {
    cat("No cases\n")
    return()
  }

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
  
  # Caption
  caption <- paste0(
    "Cases: ",
    prettyNum(yesterdayCases, big.mark = ",", scientific = FALSE),
    " (",
    prettyNum(CASES$Delta[CASES$Date == yesterday], big.mark = ",", scientific = FALSE),
    ")  --  Deaths: ",
    prettyNum(yesterdayDeaths, big.mark = ",", scientific = FALSE),
    " (",
    prettyNum(DEATHS$Delta[DEATHS$Date == yesterday], big.mark = ",", scientific = FALSE),
    # "  --  Deaths per 10,000: ",
    # round(yesterdayDeaths / Population * 10000, 1),
    ")  --  Case Mortality: ",
    sprintf("%2.1f", mortality * 100),
    "%  --  Daily Change: ",
    sprintf("%+2.1f", slopeCases),
    "% Cases, ",
    sprintf("%+2.1f", slopeDeaths),
    "% Deaths"
  )
  
  DATA$Phase <- factor(as.character(DATA$Phase), levels=c("Pre-Model","Modeled", "Deaths", "Tests"), ordered = TRUE)
  
  ggObject1 <-
    ggplot(
      DATA, 
      aes(
        x = Date, 
        y = Actual, 
        color = Phase, 
        size = Phase
      )
    ) +
    geom_point(
      na.rm = TRUE, 
      size = 1
    ) +
    coord_cartesian(
      ylim = c(1,500000000), 
      xlim = c(startDate,endDate + 2), 
      expand = TRUE, 
      clip = "on"
    ) +
    geom_line(
      data=DATA[todayIndex:nRow,], 
      size = 0.5, 
      linetype = "dotted",
      aes(y=Predicted), 
      color="red", 
      na.rm = TRUE
    ) +
    geom_line(
      data=DEATHS[todayIndex:nRow,], 
      size = 0.5,
      linetype = "dotted",
      aes(y=Predicted), 
      color="black", 
      na.rm = TRUE
    ) +
    annotate(
      "segment", 
      x = today, 
      xend = today, 
      y = 1, 
      yend = 500000000, 
      color = "blue"
    ) +
    labs(
      title = paste(Title,"projection as of", Sys.Date()),
      subtitle = Subtitle,
      y = "Cumulative"
    ) +
    scale_color_manual(
      values = c("blue","red", "black", "brown")
    ) +
    scale_size_manual (
      values = c(2,    2,       2,       1)
    ) +
    scale_x_date(
      date_breaks = "14 days",
      date_labels = "%b %d",
      minor_breaks = NULL
      
    ) +
    scale_y_log10(
      breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000,10000000, 100000000),
      labels = c("1", "10","100","1,000","10,000","100,000","1,000,000", "10,000,000","100,000,000")
    ) +
    theme(
      axis.text.x=element_text(angle=60, hjust=1),
      plot.subtitle = element_text(size = 8)
    )+
    annotation_logticks() +
    annotate(
      geom = "point",
      x = endDate,
      y = predictedCases,
      shape = 8,
      size = 1.5,
      color = "red"
    ) +
    annotate(
      geom = "text",
      x = endDate,
      y = predictedCases * 1.4,
      hjust = 0.5,
      vjust = 0,
      label = prettyNum(round(predictedCases, 0), big.mark = ",", scientific = FALSE),
      size = 3,
      color = "red"
    ) +
    annotate(
      geom = "point",
      x = endDate,
      y = predictedDeaths,
      shape = 8,
      size = 1.5,
      color = "black"
    ) +
    annotate(
      geom = "text",
      x = endDate,
      y = predictedDeaths * 1.4,
      hjust = 0.5,
      vjust = 0,
      label = prettyNum(round(predictedDeaths, 0), big.mark = ",", scientific = FALSE),
      size = 3,
      color="black"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      legend.key = element_rect(fill = NA),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Add ensemble model if US or State data are being displayed
  if (
    (is.null(State) && is.null(County) && Country == "United States of America")  |
    (is.null(Country) && is.null(County))
  )
  {
    if (is.null(State))
    {
      use <- Ensemble$Location == "US"
    } else {
      use <- Ensemble$Location == State
    }
    ggObject1 <- ggObject1 +
      geom_ribbon(
        data = as.data.frame(Ensemble[use,]),
        aes(
          x = Date,
          ymin = q.0.01,
          ymax = q.0.99
        ),
        fill = "purple",
        alpha = 0.5,
        inherit.aes = FALSE
      )
  }
  
  if (debug) print(ggObject1)
  
  maxY <- max(
    CASES$Delta_Smoothed_2[CASES$Date < today],
    DEATHS$Delta_Smoothed_2[DEATHS$Date < today] * deathAxis,
    CASES$Delta[CASES$Date < today & CASES$Phase == "Modeled"], 
    DEATHS$Delta[DEATHS$Date < today & CASES$Phase == "Modeled"] * deathAxis
  ) * 1.05
  
  CASES$DailySmoothed <- CASES$Delta_Smoothed_2
  DEATHS$DailySmoothed <- DEATHS$Delta_Smoothed_2
  CASES$DailySmoothed[CASES$DailySmoothed < 1] <- 0.8
  DEATHS$DailySmoothed[DEATHS$DailySmoothed < 1] <- 0.8
  
  CASES$DailyRaw <- CASES$Delta
  DEATHS$DailyRaw <- DEATHS$Delta
  CASES$DailyRaw[CASES$DailyRaw < 1] <- 0.8
  DEATHS$DailyRaw[DEATHS$DailyRaw < 1] <- 0.8
  
  
  ggObject2 <-
    ggplot(
      CASES, 
      aes(
        x=Date, 
        y = DailySmoothed)
    ) +  
    geom_point(
      data = CASES[CASES$Date < today & CASES$Phase == "Pre-Model",], 
      color = "blue", 
      size = 1, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    geom_point(
      data = CASES[CASES$Date < today & CASES$Phase == "Modeled",],  
      color = "red", 
      size = 1, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    # geom_point(
    #   data = CASES[CASES$Date < today & CASES$Phase == "Pre-Model",], 
    #   aes(y = Delta), 
    #   color = "blue", 
    #   size = 0.2, 
    #   shape = 3, 
    #   na.rm = TRUE, 
    #   show.legend = FALSE
    # ) +
    geom_point(
      data = CASES[CASES$Date < today & CASES$Phase == "Modeled",],  
      aes(y = DailyRaw), 
      color = "red", 
      size = 0.2, 
      shape = 3, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    geom_point(
      data = DEATHS[DEATHS$Date < today,], 
      aes(y = DailySmoothed * deathAxis), 
      color = "black", 
      size = 1, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    geom_point(
      data = DEATHS[DEATHS$Date < today & CASES$Phase == "Modeled",],  
      aes(y = DailyRaw * deathAxis), 
      color = "black", 
      size = 0.2, 
      shape = 3, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    annotate(
      "segment", 
      x = today, 
      xend = today, 
      y = 0, 
      yend = maxY, 
      color = "blue"
    ) +
    annotate(
      "segment", 
      x = today - daysLinearFitCases - 1, 
      xend = today - 1, 
      y = head(linfitCases$fitted.values,1),
      yend = tail(linfitCases$fitted.values,1),
      color = "green",
      size = 1.5
    ) +
    annotate(
      "segment", 
      x = today - daysLinearFitDeaths - 1, 
      xend = today - 1, 
      y = head(linfitDeaths$fitted.values,1) * deathAxis,
      yend = tail(linfitDeaths$fitted.values,1) * deathAxis,
      color = "green",
      size = 1.5
    ) +
    labs(
      y = "Cases and Deaths / Day",
      caption = caption
    ) +
    scale_x_date(
      date_breaks = "14 days",
      date_labels = "%b %d",
      minor_breaks = NULL
    ) +
    scale_y_log10(
      label = comma
      # ,
      # sec.axis = sec_axis(
      #   trans=~./deathAxis, 
      #   name="Deaths / Day",
      #   label = comma,
      #   ) 
    ) +
    annotation_logticks() +
    coord_cartesian(
      expand = TRUE, 
      clip = "on",
      xlim = c(startDate,endDate + 2),
      ylim = c(0.8,maxY)
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x=element_text(angle=60, hjust=1),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
    )
  
  if  (is.null(Country) || Country == "United States of America")
  {
    for (i in 1:nrow(EVENTS))
    {
      ggObject2 <- ggObject2 + 
        annotate(
          "segment", 
          x = EVENTS$Date[i], 
          xend = EVENTS$Date[i], 
          y = 0, 
          yend = maxY, 
          color = "orange"
        )  +
      annotate(
        "text", 
        label =EVENTS$Name[i],
        angle = 90,
        x = EVENTS$Date[i], 
        y = maxY, 
        color = "orange",
        size = 2,
        hjust = 1,
        vjust = -0.5
      )
    }
  }
  
  if  (Country == "Israel")
  {
      ggObject2 <- ggObject2 + 
        annotate(
          "segment", 
          x = as.Date("2020-09-18"), 
          xend = as.Date("2020-09-18"), 
          y = 0, 
          yend = maxY, 
          color = "orange"
        )  +
        annotate(
          "text", 
          label ="Second Lockdown",
          angle = 90,
          x = as.Date("2020-09-18"), 
          y = maxY, 
          color = "orange",
          size = 2,
          hjust = 1,
          vjust = -0.5
        )
  }
  
  
  
  if (debug) print(ggObject2)

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
  if(debug) print(ggObject3)
  
  nextSlide(ggObject3, Title)
  
  if (plotGrowthFlag) plotGrowth(results, Title)
  
  return(results)
}
