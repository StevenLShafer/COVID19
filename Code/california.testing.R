State <- "CA"
useCounty <- rep(TRUE, nrow(Cases_USA))
TESTS <- data.frame(
  Date = currentDates,
  Total = c(colSums(Testing_USA[Testing_USA$Abbreviation %in% State, c(4:ncol(Testing_USA))],na.rm=TRUE)),
  Data = "Tests"
)
useState <- Cases_USA$State %in% State
use <- useCounty & useState
CASES <- data.frame(
  Date = currentDates,
  Total = c(colSums(Cases_USA[use, 5:ncol(Cases_USA)],na.rm=TRUE)),
  Data = "Cases"
)
nRow <- nrow(CASES)
CASES$Delta_Smoothed <- CASES$Delta <- 0
CASES$Delta[2:nRow] <- CASES$Total[2:nRow] - CASES$Total[1:(nRow-1)]
CASES$Delta_Smoothed[4:(nRow-3)] <- rollmean(CASES$Delta, align="center",k=7)

TESTS$Delta_Smoothed <- TESTS$Delta <- 0
TESTS$Delta[2:nRow] <- TESTS$Total[2:nRow] - TESTS$Total[1:(nRow-1)]
TESTS$Delta_Smoothed[4:(nRow-3)] <- rollmean(TESTS$Delta, align="center",k=7)
DATA <- rbind(TESTS[1:(nrow-3),], CASES[1:(nrow-3),])


ggplot(DATA,aes(x=Date, y = Delta_Smoothed, color = Data)) +
  geom_line(
    data = DATA[DATA$Data == "Tests", ],
    aes(y = Delta_Smoothed / 25) , 
    size = 1, 
    na.rm = TRUE, 
    show.legend = TRUE) +
  geom_line(
    data = DATA[DATA$Data == "Cases", ], 
    aes(y = Delta_Smoothed) , 
    size = 1, 
    na.rm = TRUE, 
    show.legend = TRUE) +
  
  scale_y_continuous(
    label = comma,
    sec.axis = sec_axis(
      trans=~.*25, 
      name="Tests / Day",
      label = comma,
    ) 
  ) +
  labs(
    y = "Cases / Day"
  ) + 
  scale_x_date(
    date_breaks = "7 days",
    date_labels = "%b %d"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_text(angle=60, hjust=1)
  )
  
  

