# Plot for Rob Jackler

jacklerPlot <- function (
  DATA1,
  DATA2,
  Loc1,
  Loc2,
  title,
  caption
)
{
  
  # DATA1 <- USA
  # DATA2 <- WE
  # Loc1 <- "USA (318MM)"
  # Loc2 <- "Western Europe (344MM)"
  # title <- "Comparison of COVID-19 Cases & Deaths between US & Europe"
  
  use <- DATA1$CASES$Date  > as.Date("2020-02-29") & DATA1$CASES$Date <= today
  DATA1_CASES <- DATA1$CASES[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA1_CASES$Type <-"Cases"
  DATA1_DEATHS <- DATA1$DEATHS[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA1_DEATHS$Type <-"Deaths"
  DATA1_CASES$Location <- DATA1_DEATHS$Location <- Loc1
  
  DATA2_CASES <- DATA2$CASES[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA2_CASES$Type <-"Cases"
  DATA2_DEATHS <- DATA2$DEATHS[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA2_DEATHS$Type <-"Deaths"
  DATA2_CASES$Location <- DATA2_DEATHS$Location <- Loc2

  JacklerData <- rbind(DATA1_CASES, DATA2_CASES, DATA1_DEATHS, DATA2_DEATHS)
  
  labels <- data.frame(
    Last = round(
      JacklerData$Delta[JacklerData$Date == yesterday],
      0),
    yPOS = JacklerData$Delta_Smoothed_2[JacklerData$Date == yesterday],
    Date = today + 1,
    Type = c("Cases","Cases","Deaths","Deaths"),
    Location = c(Loc1, Loc2, Loc1, Loc2)
  )
  
  labels$Label <- prettyNum(labels$Last, big.mark = ",", scientific = FALSE)
  SignCases <- sign(labels$yPOS[1]-labels$yPOS[2])
  SignDeaths <- sign(labels$yPOS[3]-labels$yPOS[4])
  distance = 0.5
  labels$Y <- c(
    labels$yPOS[1] * exp(SignCases * distance),
    labels$yPOS[2] * exp(-SignCases * distance),
    labels$yPOS[3] * exp(SignDeaths * distance),
    labels$yPOS[4] * exp(-SignDeaths * distance)
  )
  
  X <- abs(log(DATA1_CASES$Delta_Smoothed_2) - log(DATA2_CASES$Delta_Smoothed_2))
  X <- which (max(X[100:length(X)]) == X)
  caseLabelX <- DATA1_CASES$Date[X]
  caseLabelY <- exp((log(DATA1_CASES$Delta_Smoothed_2[X]) + log(DATA2_CASES$Delta_Smoothed_2[X]))/2)
  
  X <- abs(log(DATA1_DEATHS$Delta_Smoothed_2) - log(DATA2_DEATHS$Delta_Smoothed_2))
  X <- which (max(X[100:length(X)]) == X)
  deathLabelX <- DATA1_DEATHS$Date[X]
  deathLabelY <- exp((log(DATA1_DEATHS$Delta_Smoothed_2[X]) + log(DATA2_DEATHS$Delta_Smoothed_2[X]))/2)
  
  ggObject <- ggplot(JacklerData, aes(x = Date, y = Delta_Smoothed_2, color = Location, linetype = Type)) +
    geom_line(size = 1) +
    scale_x_date(
      date_breaks = "14 days",
      date_labels = "%b %d",
      minor_breaks = NULL
    ) +
    scale_y_log10(
      breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000),
      labels = c("1", "3","10", "30","100","300","1,000","3,000", "10,000", "30,000","100,000", "300,000")
    ) +
    coord_cartesian(ylim = c(1, 300000)) +
    scale_color_manual(values = c("dodgerblue","coral")) +
    labs(
      y = "Daily Cases and Deaths",
      title = title, 
      subtitle = "Log plot of 7 day average",
      caption = caption
    ) +
    annotation_logticks() + 
    scale_linetype_manual(values = c("solid","solid")) +
    annotate(
      geom = "text",
      label = "Cases",
      x = caseLabelX,
      y = caseLabelY
    ) +
    annotate(
      geom = "text",
      label = "Deaths",
      x = deathLabelX,
      y = deathLabelY
    ) +
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
    ) +
    guides(linetype = FALSE)
  nextSlide(ggObject, title)
  
  emailText <<- paste(
    emailText,
    email.list.start,
    "Daily cases and deaths in",
    Loc1,
    "versus",
    Loc2,
    sls_trim(
      add_ggplot(
        plot_object = ggObject,
        width = 9.5,
        height = 5,
        alt = NULL,
        align = "left",
        float = "none"
      )
    ),
    email.list.end
  )
}


shaferPlot <- function (
  DATA1,
  DATA2,
  DATA3,
  Loc1,
  Loc2,
  Loc3,
  title,
  caption, 
  ylabel
)
{
  
  # DATA1 <- USA$CASES
  # DATA2 <- WE$CASES
  # DATA3 <- ASIA$CASES
  # Loc1 <- "USA (318MM)"
  # Loc2 <- "Western Europe (355MM)"
  # Loc3 <- "Asian Ensemble (328MM)"
  # title <- "Comparison of COVID-19 Cases between US, Europe, and Asia"
  # caption <- "hello"
  # ylabel <- "Cases"
  
  use <- DATA1$Date  > as.Date("2020-02-29") & DATA1$Date <= today
  DATA1 <- DATA1[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA1$Location <- Loc1

  use <- DATA2$Date  > as.Date("2020-02-29") & DATA2$Date <= today
  DATA2 <- DATA2[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA2$Location <- Loc2
  
  use <- DATA3$Date  > as.Date("2020-02-29") & DATA3$Date <= today
  DATA3 <- DATA3[use,c("Date", "Delta", "Delta_Smoothed_2")]
  DATA3$Location <- Loc3
  
  JacklerData <- rbind(DATA1, DATA2, DATA3)
  
  labels <- data.frame(
    Last = round(
      JacklerData$Delta[JacklerData$Date == yesterday],
      0),
    yPOS = JacklerData$Delta_Smoothed_2[JacklerData$Date == yesterday],
    Date = today + 1,
    Location = c(Loc1, Loc2, Loc3)
  )
  
  labels$Label <- prettyNum(labels$Last, big.mark = ",", scientific = FALSE)
  Sign <- sign(labels$yPOS[1]-labels$yPOS[2])
  distance = 0.5
  labels$Y <- c(
    labels$yPOS[1] * exp(Sign * distance),
    labels$yPOS[2] * exp(-Sign * distance),
    labels$yPOS[3] * exp(1 * distance)
  )
  
  ggObject <- ggplot(JacklerData, aes(x = Date, y = Delta_Smoothed_2, color = Location)) +
    geom_line(size = 1) +
    scale_x_date(
      date_breaks = "14 days",
      date_labels = "%b %d",
      minor_breaks = NULL
    ) +
    scale_y_log10(
      breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000),
      labels = c("1", "3","10", "30","100","300","1,000","3,000", "10,000", "30,000","100,000", "300,000")
    ) +
    coord_cartesian(ylim = c(1, 300000)) +
  scale_color_manual(values = c("forestgreen", "dodgerblue","coral")) +
    labs(
      y = paste("Daily", ylabel),
      title = title, 
      subtitle = "Log plot of 7 day average",
      caption = caption
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
    ) +
    guides(linetype = FALSE)
  nextSlide(ggObject, title)
  
  emailText <<- paste0(
    emailText,
    " ",
    email.list.start,
    " Daily ",
    tolower(ylabel),
    " in ",
    Loc1,
    ", ",
    Loc2,
    ", and ",
    Loc3,
    ". ",
    sls_trim(
      add_ggplot(
        plot_object = ggObject,
        width = 9.5,
        height = 5,
        alt = NULL,
        align = "left",
        float = "none"
      )
    ),
    " ",
    email.list.end
  )
}
