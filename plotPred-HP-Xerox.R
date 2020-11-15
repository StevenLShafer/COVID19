# plotPred function - the main workhorse function for the analysis
plotPred <- function(
  County = NULL,
  State = NULL,
  Country = NULL,
  Title = NULL,
  modelStart = NULL,
  weight = NULL
)
{
  debug <- FALSE
  if (!exists("modelStart"))
  {
    County <- NULL
    State <- "AL"
    Country <- NULL
    Title <- NULL
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
  
#  CASES$Delta_Smoothed_2[3:(nRow-2)] <- rollmean(CASES$Delta_Smoothed, align="center",k=5)
#  CASES$Delta_Smoothed_2[(nRow-2):nrow(CASES)] <- CASES$Delta_Smoothed_2[nRow-3]
  

  
  # Caption
  caption <- paste0(
    "Cases: ",
    prettyNum(yesterdayCases, big.mark = ",", scientific = FALSE),
    "  --  Deaths: ",
    prettyNum(yesterdayDeaths, big.mark = ",", scientific = FALSE),
    # "  --  Deaths per 10,000: ",
    # round(yesterdayDeaths / Population * 10000, 1),
    "  --  Case Mortality: ",
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
      ylim = c(1,100000000), 
      expand = TRUE, 
      clip = "on"
    ) +
    geom_line(
      data=DATA[DATA$Phase == "Modeled",], 
      size = 1, 
      aes(y=Predicted), 
      color="red", 
      na.rm = TRUE
    ) +
    geom_line(
      data=DEATHS[todayIndex:nRow,], 
      size = 1, 
      aes(y=Predicted), 
      color="black", 
      na.rm = TRUE
    ) +
    annotate(
      "segment", 
      x = today, 
      xend = today, 
      y = 1, 
      yend = 100000000, 
      color = "blue"
    ) +
    labs(
      title = paste(Title,"projection as of", Sys.Date()),
      y = "Actual (points) / Predicted (line)"
    ) +
    scale_color_manual(
      values = c("blue","red", "black", "brown")
    ) +
    scale_size_manual (
      values = c(2,    2,       2,       1)
    ) +
    scale_x_date(
      date_breaks = "7 days",
      date_labels = "%b %d"
    ) +
    scale_y_log10(
      breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000,10000000, 100000000),
      labels = c("1", "10","100","1,000","10,000","100,000","1,000,000", "10,000,000","100,000,000")
    ) +
    theme(
      axis.text.x=element_text(angle=60, hjust=1)
    )+
    annotation_logticks() +
    annotate(
      geom = "point",
      x = endDate,
      y = predictedCases,
      shape = 8,
      size = 2,
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
      size = 2,
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
  if (debug) print(ggObject1)
  
  ggObject2 <-
    ggplot(
      CASES, 
      aes(
        x=Date, 
        y = Delta_Smoothed_2)
    ) +  
    geom_point(data = CASES[CASES$Date < today & CASES$Phase == "Pre-Model",], color = "blue", size = 1, na.rm = TRUE, show.legend = FALSE) +
    geom_point(data = CASES[CASES$Date < today & CASES$Phase == "Modeled",],  color = "red", size = 1, na.rm = TRUE, show.legend = FALSE) +
    geom_line(
      data = CASES[CASES$Date >= today,], 
      size = 1, 
      na.rm = TRUE, 
      show.legend = FALSE, 
      color = "red"
    ) +
    geom_point(
      data = DEATHS[DEATHS$Date < today,], 
      aes(y = Delta_Smoothed * 5), 
      color = "black", 
      size = 1, 
      na.rm = TRUE, 
      show.legend = FALSE
    ) +
    annotate(
      "segment", 
      x = today, 
      xend = today, 
      y = 0, 
      yend = max(CASES$Delta_Smoothed_2 * 1.1), 
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
      y = head(linfitDeaths$fitted.values,1) * 5,
      yend = tail(linfitDeaths$fitted.values,1) * 5,
      color = "green",
      size = 1.5
    ) +
    labs(
      y = "Cases / Day",
      caption = caption
    ) +
    scale_x_date(
      date_breaks = "7 days",
      date_labels = "%b %d"
    ) +
    scale_y_continuous(
      label = comma,
      sec.axis = sec_axis(
        trans=~./5, 
        name="Deaths / Day",
        label = comma,
        ) 
    ) +
    coord_cartesian(expand = TRUE, clip = "on") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x=element_text(angle=60, hjust=1),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
    )

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
  
  return(predictedDeaths)
}
