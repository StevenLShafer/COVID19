# calcStats, calculation the main statistics in the analysis
calcStats <- function(
  County = NULL,
  State = NULL,
  Country = NULL,
  modelStart = NULL,
  weight = 0
)
{
  
  # Set defaults for debugging purposes
  if (!exists("modelStart"))
  {
    County <- NULL
    State <- NULL
    Country <- "United States of America"
    Title <- "Test"
    debug <- TRUE
    weight <- 0
    modelStart <- NULL 
  }
  if (is.null(modelStart)) modelStart <- today - daysGompertzFit
  
  if (!is.null(Country))
  {
    # Global data
    use <- Cases_Global$Country %in% Country
    CASES <- data.frame(
      Date = allDates,
      Actual = c(colSums(Cases_Global[use, c(2:ncol(Cases_Global))],na.rm=TRUE), rep(NA, projection)),
      Phase = "",
      Predicted = NA,
      stringsAsFactors = FALSE
    )
    DEATHS <- data.frame(
      Date = allDates,
      Actual = c(colSums(Deaths_Global[use, c(2:ncol(Deaths_Global))],na.rm=TRUE), rep(NA, projection)),
      Phase = "Deaths",
      Predicted = NA,
      stringsAsFactors = FALSE
    )
    TESTS <- data.frame(
      Date = currentDates,
      Actual = c(colSums(Testing_Global[Testing_Global$country %in% Country, c(3:ncol(Testing_Global))],na.rm=TRUE)),
      Phase = "Tests",
      Predicted = NA,
      stringsAsFactors = FALSE
    )
    Population <- sum(Population_Global$Population[Population_Global$Country %in% Country], na.rm=TRUE) 
    maxCases <-  Population / (1 + asymptomatic)
  } else {
    # USA Data  
    if(is.null(County))
    {
      # Just state data
      useCounty <- rep(TRUE, nrow(Cases_USA))
      TESTS <- data.frame(
        Date = currentDates,
        Actual = c(colSums(Testing_USA[Testing_USA$Abbreviation %in% State, c(4:ncol(Testing_USA))],na.rm=TRUE)),
        Phase = "Tests",
        Predicted = NA,
        stringsAsFactors = FALSE
      )
    } else {
      # County of interest
      useCounty <- Cases_USA$County.Name %in% County
      TESTS <- NULL
    }
    if(is.null(State))
    {
      # If no state is specified, then use all states
      useState <- rep(TRUE, nrow(Cases_USA))
    } else {
      # Just state specified
      useState <- Cases_USA$State %in% State
    }
      
    use <- useCounty & useState
    CASES <- data.frame(
      Date = allDates,
      Actual = c(colSums(Cases_USA[use, 5:ncol(Cases_USA)],na.rm=TRUE), rep(NA, projection)),
      Phase = "",
      Predicted = NA,
      stringsAsFactors = FALSE
    )
    DEATHS <- data.frame(
      Date = allDates,
        Actual = c(colSums(Deaths_USA[use, c(5:ncol(Deaths_USA))],na.rm=TRUE), rep(NA, projection)),
        Phase = "Deaths",
        Predicted = NA,
        stringsAsFactors = FALSE
      )
      FIPS <- Cases_USA$CountyFIPS[use]
      Population <- sum(Population_USA$Population[Population_USA$CountyFIPS %in% FIPS], na.rm=TRUE)
      maxCases <- Population / (1 + asymptomatic)
  }
  
  # Cumulative case numbers, deaths, and tests cannot drop
  nRow <- nrow(CASES)
  modelStartIndex   <- which(CASES$Date == modelStart)
  todayIndex        <- which(CASES$Date == today)
  for (i in 1:(nRow-1))
  {
    if(!is.na(CASES$Actual[i+1]))
      if (CASES$Actual[i]  > CASES$Actual[i+1] ) CASES$Actual[i+1] <- CASES$Actual[i]
      if(!is.na(DEATHS$Actual[i+1]))
        if (DEATHS$Actual[i] > DEATHS$Actual[i+1]) DEATHS$Actual[i+1] <- DEATHS$Actual[i]
  }
  if (!is.null(TESTS))
  {
    for (i in nrow(TESTS):2)
    {
      if(!is.na(TESTS$Actual[i]))
        if (TESTS$Actual[i-1] > TESTS$Actual[i]) TESTS$Actual[i-1] <- TESTS$Actual[i]
    }
  }
  
  if (sum(CASES$Actual, na.rm = TRUE) == 0) return(NULL)
  
  # Clean up CASES and DEATHS
  CASES$Phase[1:(modelStartIndex-1)] <- "Pre-Model"
  CASES$Phase[modelStartIndex:nRow] <- "Modeled"
  CASES$Actual[CASES$Actual == 0 | is.na(CASES$Actual)] <- NA
  DEATHS$Actual[DEATHS$Actual == 0 | is.na(DEATHS$Actual)] <- NA
  #if(sum(!is.na(DEATHS$Actual) == 0)) DEATHS$Actual <- 0.1
  maxCases <- log(maxCases)
  
  # Fit current data
  use <- CASES$Date >= modelStart & !is.na(CASES$Actual) & CASES$Actual > 0
  if (sum(use) > 5)
  {
    Y <- log(CASES$Actual[use])
    fitGom <- Gompertz_fit(Y, maxCases, weight)
  } else
  {
    fitGom <- NULL
  }
  
  # Predict future cases
  X <- modelStartIndex:nRow
  L <- length(X)
  
  if (sum(use) > 5)
  {
    CASES$Predicted[X] <- exp(Gompertz_fn(fitGom, L))
  } else {
    CASES$Predicted[X] <- CASES$Actual[todayIndex - 1]
  }
  predictedCases <- tail(CASES$Predicted, 1)
  mortality <-   DEATHS$Actual[todayIndex - 1] / CASES$Actual[todayIndex - 1]
  predictedDeaths <- predictedCases * mortality
  
  if (sum(DEATHS$Actual, na.rm = TRUE) > 0)
  {
    DATA <- rbind(CASES, DEATHS)
  } else {
    DATA <- CASES
  }
  if (!is.null(TESTS))
  {
    TESTS <- TESTS[TESTS$Actual > 0,]
    DATA <- rbind(DATA, TESTS)
  }
  
  # Daily New Cases
  CASES$Total <- CASES$Actual
  CASES$Total[todayIndex:nRow] <- CASES$Predicted[todayIndex:nRow]
  CASES$Total[is.na(CASES$Total)] <- 0
  CASES$Delta_Smoothed_2 <- CASES$Delta_Smoothed <- CASES$Delta <- 0
  CASES$Delta[2:nRow] <- CASES$Total[2:nRow] - CASES$Total[1:(nRow-1)]
  CASES$Delta[todayIndex] <- (CASES$Delta[todayIndex - 1] + CASES$Delta[todayIndex + 1])/ 2
  CASES$Delta_Smoothed[3:(nRow-2)] <- rollmean(CASES$Delta, align="center",k=5)
  CASES$Delta_Smoothed[(nRow-2):nRow] <- CASES$Delta_Smoothed[nRow-3]
  
#  CASES$Source <- "Reported"
#  CASES$Source <- "Predicted"
  
  # Calculate line of fit
  CASES$Delta_Smoothed_2[3:(nRow-2)] <- rollmean(CASES$Delta_Smoothed, align="center",k=5)
  CASES$Delta_Smoothed_2[(nRow-2):nRow] <- CASES$Delta_Smoothed_2[nRow-3]
  X <- (todayIndex-daysLinearFitCases - 1):(todayIndex-1)
  linfitCases  <- lm(CASES$Delta_Smoothed_2[X] ~ X)
  slopeCases <- linfitCases$coefficients[2] / mean(CASES$Delta_Smoothed_2[X]) * 100

  # Daily New Deaths
  DEATHS$Total <- DEATHS$Actual
  DEATHS$Total[todayIndex:nRow] <- DEATHS$Predicted[todayIndex:nRow]
  DEATHS$Total[is.na(DEATHS$Total)] <- 0
  DEATHS$Delta_Smoothed_2 <- DEATHS$Delta_Smoothed <- DEATHS$Delta <- 0
  DEATHS$Delta[2:nRow] <- DEATHS$Total[2:nRow] - DEATHS$Total[1:(nRow-1)]
  DEATHS$Delta[todayIndex] <- (DEATHS$Delta[todayIndex - 1] + DEATHS$Delta[todayIndex + 1])/ 2
  DEATHS$Delta_Smoothed[3:(nRow-2)] <- rollmean(DEATHS$Delta, align="center",k=5)
  DEATHS$Delta_Smoothed[(nRow-2):nRow] <- DEATHS$Delta_Smoothed[nRow-3]
  
#  CASES$Source <- "Reported"
#  CASES$Source <- "Predicted"
  
  # Calculate line of fit
  DEATHS$Delta_Smoothed_2[3:(nRow-2)] <- rollmean(DEATHS$Delta_Smoothed, align="center",k=5)
  DEATHS$Delta_Smoothed_2[(nRow-2):nRow] <- DEATHS$Delta_Smoothed_2[nRow-3]
  X <- (todayIndex-daysLinearFitDeaths - 1):(todayIndex-1)
  linfitDeaths  <- lm(DEATHS$Delta_Smoothed_2[X] ~ X)
  slopeDeaths <- linfitDeaths$coefficients[2] / mean(CASES$Delta_Smoothed_2[X]) * 100
  
  
  return(
    list(
      DATA = DATA,
      CASES = CASES,
      DEATHS = DEATHS,
      predictedCases = predictedCases,
      predictedDeaths = predictedDeaths,
      mortality = mortality,
      fitGom = fitGom,
      linfitCases = linfitCases,
      linfitDeaths = linfitDeaths,
      slopeCases = slopeCases,
      slopeDeaths = slopeDeaths,
      Population = Population,
      todayIndex = todayIndex,
      yesterdayCases = CASES$Actual[todayIndex - 1],
      yesterdayDeaths = DEATHS$Actual[todayIndex - 1],
      nRow = nRow
    )
  )
}
