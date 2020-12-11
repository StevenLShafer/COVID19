# plotPred function - the main workhorse function for the analysis
plotPred <- function(
  County = NULL,
  State = NULL,
  Country = NULL,
  Title = NULL,
  Subtitle = NULL,
  modelStart = NULL,
  weight = NULL,
  addPlot = FALSE
)
{
  options(repr.plot.width = 9, repr.plot.height = 5)
  debug <- FALSE
  if (!exists("modelStart"))
  {
    County <- NULL
    State <- "MA"
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
    prettyNum(round(yesterdayCases / Population * 100,1), big.mark = ",", scientific = FALSE),
    "%, 1 in ",
    prettyNum(round(Population / yesterdayCases, 0), big.mark = ",", scientific = FALSE),
    ")  --  Deaths: ",
    prettyNum(yesterdayDeaths, big.mark = ",", scientific = FALSE),
    " (",
    prettyNum(round(yesterdayDeaths / Population * 100,2), big.mark = ",", scientific = FALSE),
    "%, 1 in ",
    prettyNum(round(Population / yesterdayDeaths,0), big.mark = ",", scientific = FALSE),
    ")  --  Case Mortality: ",
    sprintf("%2.1f", mortality * 100),
    "%\n",
    "Daily change (averaged over ",
    daysLinearFit,
    " days) -- Cases: ",
    sprintf("%+2.1f", slopeCases),
    "% per day, Deaths: ",
    sprintf("%+2.1f", slopeDeaths),
    "% per day"
  )
  
  DATA$Legend <- factor(
    as.character(
      DATA$Legend), 
    levels=c("Cases", "Deaths", "Tests"), 
    ordered = TRUE
    )
  
  DATA$Actual[CASES$Actual < 1] <- 0.1

  ggObject1 <-
    ggplot(
      DATA, 
      aes(
        x = Date, 
        y = Actual, 
        color = Legend, 
        size = Legend
      )
    ) +
    geom_point(
      na.rm = TRUE, 
      size = 1
    ) +
    coord_cartesian(
      ylim = c(0.8,Population * 10), 
      xlim = c(startDate,endDate + 16), 
      expand = FALSE, 
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
      yend = 50000000000, 
      color = "blue"
    ) +
    labs(
      title = paste(Title,"projection as of", Sys.Date()),
      subtitle = Subtitle,
      y = "Cumulative"
    ) +
    scale_color_manual(
      values = c("blue", "black", "brown")
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
      breaks = c(10^(0:10)),
      labels = comma_format(
        big.mark = ',',
        accuracy = 1),
      sec.axis = dup_axis(
        trans = ~Population / .,
        breaks = 10^(0:10),
        labels = paste(
          "1 in", 
          prettyNum(10^(0:10), big.mark = ",", scientific=FALSE))
        )
    ) +
    theme(
      axis.text.x=element_text(angle=60, hjust=1),
      plot.subtitle = element_text(size = 8),
      axis.title.y.right = element_blank()
    )+
    annotation_logticks() +
    annotate(
      "segment", 
      x = startDate, 
      xend = today, 
      y = Population, 
      yend = Population, 
      color = "black"
    ) +
    annotate(
      geom = "text",
      x = startDate + 15,
      y = Population * 1.4,
      hjust = 0,
      vjust = 0,
      label = paste("Population:", prettyNum(Population, big.mark = ",", scientific = FALSE)),
      size = 3,
      color = "black"
    ) +
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
      size = 2.7,
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
      size = 2.7,
      color="black"
    ) +
    annotate(
      geom = "point",
      x = startDate+15,
      y = Population / 16,
      size = 1.5,
      color="blue"
    ) +
    annotate(
      geom = "point",
      x = startDate+15,
      y = Population / 64,
      size = 1.5,
      color="black"
    ) +
    annotate(
      geom = "text",
      x = startDate + 20,
      y = Population / 16,
      hjust = 0,
      vjust = 0.5,
      label = "Cases",
      size = 2,
      color="blue"
    ) +
    annotate(
      geom = "text",
      x = startDate + 20,
      y = Population / 64,
      hjust = 0,
      vjust = 0.5,
      label = "Deaths",
      size = 2,
      color="black"
    ) +
    
    theme(
      panel.grid.minor = element_blank(),
      legend.key = element_rect(fill = NA),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
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
  if (sum(DATA$Legend == "Tests") > 0)
  {
    ggObject1 <- ggObject1 + 
      annotate(
        geom = "point",
        x = startDate+15,
        y = Population / 4,
        size = 1.5,
        color="brown"
      ) +
      annotate(
        geom = "text",
        x = startDate + 20,
        y = Population / 4,
        hjust = 0,
        vjust = 0.5,
        label = "Tests",
        size = 2,
        color="brown"
      )
    }
    
    if (debug) print(ggObject1)

  maxY <- max(
    CASES$Delta_Smoothed_2[CASES$Date < today],
    DEATHS$Delta_Smoothed_2[DEATHS$Date < today],
    CASES$Delta[CASES$Date < today & CASES$Date >= today - daysShowRaw], 
    DEATHS$Delta[DEATHS$Date < today & CASES$Date >= today - daysShowRaw]
  ) * 2
  maxY <- 10^ceiling(log10(maxY))
  
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
      data = CASES[CASES$Date < today & CASES$Legend == "Cases",],
      color = "blue",
      size = 1,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    geom_point(
      data = CASES[CASES$Date < today & CASES$Date >= today - daysShowRaw,],  
      aes(y = DailyRaw), 
      color = "blue", 
      size = 0.2, 
      shape = 3, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    
    geom_point(
      data = DEATHS[DEATHS$Date < today,], 
      aes(y = DailySmoothed), 
      color = "black", 
      size = 1, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    geom_point(
      data = DEATHS[DEATHS$Date < today & DEATHS$Date >= today - daysShowRaw,],  
      aes(y = DailyRaw), 
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
      y = 1, 
      yend = maxY, 
      color = "blue"
    ) +
    annotate(
      "segment", 
      x = today - daysLinearFit - 1, 
      xend = today - 1, 
      y = head(linfitCases$fitted.values,1),
      yend = tail(linfitCases$fitted.values,1),
      color = "green",
      size = 1.5
    ) +
    annotate(
      "segment", 
      x = today - daysLinearFit - 1, 
      xend = today - 1, 
      y = head(linfitDeaths$fitted.values,1),
      yend = tail(linfitDeaths$fitted.values,1),
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
      breaks = c(10^(0:10)),
      labels = comma_format(
        big.mark = ',',
        accuracy = 1),
      sec.axis = dup_axis(
        trans = ~Population / .,
        breaks = 10^(0:10),
        labels = paste(
          "1 in", 
          prettyNum(10^(0:10), big.mark = ",", scientific=FALSE))
      )
    ) +
    annotation_logticks() +
    coord_cartesian(
      expand = FALSE, 
      clip = "on",
      xlim = c(startDate,endDate + 16),
      ylim = c(0.8,maxY)
    ) +
    annotate(
      geom = "text",
      x = today + 2,
      y = CASES$Delta[DEATHS$Date == yesterday],
      hjust = 0,
      vjust = 0.5,
      label = prettyNum(round(CASES$Delta[DEATHS$Date == yesterday], 0), big.mark = ",", scientific = FALSE),
      size = 3,
      color="blue"
    ) +
    annotate(
      geom = "text",
      x = today + 2,
      y = DEATHS$Delta[DEATHS$Date == yesterday],
      hjust = 0,
      vjust = 0.5,
      label = prettyNum(round(DEATHS$Delta[DEATHS$Date == yesterday], 0), big.mark = ",", scientific = FALSE),
      size = 3,
      color="black"
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x=element_text(angle=60, hjust=1),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.y.right = element_blank()
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
          y = 1, 
          yend = maxY, 
          color = "orange"
        )  +
      annotate(
        "text", 
        label =EVENTS$Name[i],
        angle = 90,
        x = EVENTS$Date[i], 
        y = 1.2, 
        color = "orange",
        size = 2,
        hjust = 0,
        vjust = -0.5
      )
    }
  }
  
  if  (!is.null(Country) && length(Country) == 1 && Country == "Israel")
  {
      ggObject2 <- ggObject2 + 
        annotate(
          "segment", 
          x = as.Date("2020-09-18"), 
          xend = as.Date("2020-09-18"), 
          y = 1, 
          yend = maxY, 
          color = "orange"
        )  +
        annotate(
          "text", 
          label ="Second Lockdown",
          angle = 90,
          x = as.Date("2020-09-18"), 
          y = 1.2, 
          color = "orange",
          size = 2,
          hjust = 0,
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
  
  if (addPlot)
  {
    emailText <<- paste(
      emailText,
      email.list.start,
      "Today's time series for", 
      Title,
      ".",
      sls_trim(
        add_ggplot(
          plot_object = ggObject3,
          width = 9,
          height = 5,
          alt = NULL,
          align = "left",
          float = "none"
        )
      ),
      email.list.end
    )
  
  }
  
  return(list(
    ggObject = ggObject3, 
    results = results
    )
  )
}
