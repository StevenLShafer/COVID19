# Plot Mortality
plotMortality <- function(
  County = NULL,
  State = NULL,
  Country = NULL,
  Title = NULL
 )
{
  modelStart <- today - daysGompertzFit  
  # Country <- "United States of America"
  # State <- NULL
  # County <- NULL
  # Title <- "US"
  results <- calcStats(
    County = County,
    State = State,
    Country = Country,
    modelStart = modelStart,
    weight = 0
  )

  FIRST <- which (results$DEATH$Actual > 0)[1]
  LAST <- which(results$CASES$Date == today-8)
  Cumulative_Cases  <- results$CASES$Actual[FIRST:LAST]
  Cumulative_Deaths <- results$DEATHS$Actual[FIRST:LAST]
  Daily_Cases <- results$CASES$Delta[FIRST:LAST]
  Daily_Deaths <- results$DEATHS$Delta[FIRST:LAST]
  
Daily_mortality <- as.numeric(Daily_Deaths / Daily_Cases) * 100
Cumulative_Mortality <-  Cumulative_Deaths / Cumulative_Cases * 100

L <- length(Daily_mortality)
Dates <- currentDates[FIRST:LAST]
window <- 13
offset <- (window - 1)/2
level <- paste(window, "day rolling median")
Z <- rollmedian (Daily_mortality, window)
C <- Cumulative_Deaths / Cumulative_Cases * 100

D <- data.frame(
  Date = c(Dates, Dates[(offset + 1):(L-offset)], currentDates[FIRST:LAST]),
  Mortality = c(Daily_mortality, Z, C),
  Type = level
)
D$Type[1:length(Dates)] <- "Daily Raw"
D$Type[(nrow(D) - (LAST-FIRST )):nrow(D)] <- "Cumulative"

D$Type <- factor(D$Type, levels = c("Daily Raw", level, "Cumulative" ), ordered = TRUE)

D <- D[!is.na(D$Mortality),]
yMax <- max(C, Z, na.rm = TRUE) * 1.5

ggObject <- ggplot(
  D,
  aes(
    x = Date, 
    y = Mortality, 
    color = Type
    )
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
  ) +
  scale_y_continuous(limits = c(0, yMax)
  ) +
  scale_x_date(
    date_breaks = "14 days",
    date_labels = "%b %d"
  ) +
  scale_color_manual(
    values = c("green","red","black")
  ) +
  labs(
    y = "Case Rate Mortality (%)",
    title = paste(Title, "Daily vs. Cumulative Case Rate Mortality as of", today),
    caption = "Last week excluded because deaths are often backdated",
    color = "Mortality Type:"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=60, hjust=1),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
  )
nextSlide(ggObject, paste(Title, "Mortality Trends"))
}
