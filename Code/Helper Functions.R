# Helper Functions

# Set up PowerPoint
timestamp <- format(Sys.time(), format = "%Y-%m-%d")
master <- "Office Theme"
slideNumber <- 3
pptx <- NULL # Define it so I can update it below
pptxfileName <- NULL
subSet <- ""
datetime <- as.character(today)

email.list.start <- "\n<li class=\"SLSListParagraph\">\n"
email.list.end <- "\n</li>\n"

textSummary <- function(X, title)
{
  deaths <- X$DEATHS$Delta[X$DEATHS$Date == yesterday]
  if (deaths == 1)
  {
    deathText = " death"
  } else {
    deathText = " deaths"
  }
  text <- paste0(
    title,
    " there were ",
    prettyNum(
      X$CASES$Delta[X$CASES$Date == yesterday],
      big.mark = ",", 
      scientific = FALSE),
    " new cases and ",
    prettyNum(
      deaths,
      big.mark = ",", 
      scientific = FALSE),
    deathText,
    " yesterday (Summary: ",
    slideNumber - 1,
    ")."
    )
  if (abs(X$slopeCases) < 0.1)
  {
    slopeCasesText <- "The daily case rate is essentially unchanged and"
  } else {
    if (X$slopeCases > 0)
    {
      slopeCasesText <- paste0("Daily cases are increasing ", round(X$slopeCases,1),"% and")
    } else {
      slopeCasesText <- paste0("New cases are decreasing ", round(-X$slopeCases,1),"% and")
    }
  }
  if (abs(X$slopeDeaths) < 0.1)
  {
    slopeDeathsText <- "the daily death rate is essentially unchanged"
  } else {
    if (X$slopeDeaths > 0)
    {
      slopeDeathsText <- paste0("deaths are increasing ", round(X$slopeDeaths,1),"%")
    } else {
      slopeDeathsText <- paste0("deaths are decreasing ", round(-X$slopeDeaths,1),"%")
    }
  }
  return(
    paste(
      emailText,
      email.list.start,
      text,
      slopeCasesText,
      slopeDeathsText,
      "over the past",
      daysLinearFit,
      "days.",
      email.list.end
    )
  )
}

textRanksInternational <- function(X, title, N, addPlot, ggObject)
{
  USA <- which(X$Abbreviation == "USA")
  if (X$Rank[USA] == 1)
  {
    X$Country[USA] <- "The USA"
  } else {
    X$Country[USA] <- "the USA"
  }
  text <- paste0(
    X$Country[X$Rank == 1],
    " has the most ",
    title,
    ", followed by "
    )
  for (i in 2:(N-1))
  {
    text <- paste0(
      text,
      X$Country[X$Rank == i],
      ", "
    )
  }
  text <- paste0(
    text,
    "and ",
    X$Country[X$Rank == N],
    " (Summary: ",
    slideNumber - 1,
    ")."
  )
  if (addPlot)
  {
    returnText <-     paste(
      emailText,
      email.list.start,
      text,
      add_ggplot(
        plot_object = ggObject,
        width = 7.2, # was 9
        height = 4.5,
        alt = NULL,
        align = "left",
        float = "none"
      ),
      email.list.end
    )
  }
  else
  {
    returnText <-     paste(
      emailText,
      email.list.start,
      text,
      email.list.end
    )
  }
  
  return(returnText)
}

textRanksStates <- function(X, title, N, addPlot, ggObject)
{
  if (title == "percent tested")
  {
    comparator = " highest "
  } else {
    comparator = " most "
  }
  text <- paste0(
    X$State[X$Rank == 1],
    " has the",
    comparator,
    title,
    ", followed by "
  )
  for (i in 2:(N-1))
  {
    text <- paste0(
      text,
      X$State[X$Rank == i],
      ", "
    )
  }
  text <- paste0(
    text,
    "and ",
    X$State[X$Rank == N],
    " (Summary: ",
    slideNumber - 1,
    ")."
  )
  
  if (addPlot)
  {
    returnText <-     paste(
      emailText,
      email.list.start,
      text,
      add_ggplot(
        plot_object = ggObject,
        width = 8, # was 9
        height = 4.5, # was 9
        alt = NULL,
        align = "left",
        float = "none"
      ),
      email.list.end
    )
  }
  else
  {
    returnText <-     paste(
      emailText,
      email.list.start,
      text,
      email.list.end
    )
  }
  return(returnText)
}

nextSlide <- function (ggObject, Title)
{
  suppressWarnings(
    print(ggObject)
  )
  
  pptx <<- add_slide(pptx, layout = "Title and Content", master = master)
  pptx <<- ph_with(pptx, value = Title, location = ph_location_type("title"))
  suppressWarnings(
    pptx <<- ph_with(pptx, value = dml(ggobj = ggObject), location = ph_location_type("body"))
  )
  pptx <<- ph_with(pptx, value = datetime, location = ph_location_type("dt"))
  pptx <<- ph_with(pptx, value = paste(subSet, slideNumber), location = ph_location_type("sldNum"))
  slideNumber <<- slideNumber + 1
}

newSection <- function (Title)
{
  subSet <<- paste0(Title,":")
  if (Title == "Summary")
  {
    pptx <<- read_pptx(paste0(dirSheets, "Template.new.pptx"))
    slideNumber <<- 3
  } else {
    pptx <<- read_pptx(paste0(dirSheets, "Template.blank.pptx"))
    pptx <<- add_slide(pptx, layout = "Section Header", master = master)
    pptx <<- ph_with(pptx, value = Title, location = ph_location_type("title"))
    pptx <<- ph_with(
      pptx, 
      value = paste(subSet, 1), 
      location = ph_location_type("sldNum"))
    slideNumber <<- 2
  }
  
  if (plotGrowthFlag) 
  {
    pptxfileName <<- paste0(
      dirTodayUpdate, 
      "Steve's COVID Update.plotGrowth.", 
      Title, 
      ".", 
      timestamp, 
      ".pptx"
    )
  } else {
    pptxfileName <<- paste0(
      dirTodayUpdate, 
      "Steve's COVID Update.", 
      Title, 
      ".", 
      timestamp, 
      ".pptx"
    )
  }
  
  if (file.exists(pptxfileName))
    file.remove(pptxfileName)
  while(file.exists(pptxfileName))
  {
    cat("Close the open PowerPoint File\n")
    file.remove(pptxfileName)
  }
}

closest <- function(a, b)
{
  which(abs(a-b) == min(abs(a-b), na.rm=TRUE))[1]
}

Gompertz_fn <- function(par, N)
{
  # par <- intercept, peak, k
  return(par[1] + (par[2] - par[1]) * (1-exp(-par[3] * 0:(N-1))))
}

Gompertz_obj <- function(par, Y, weight)
{
  return(
    sum(
      (Y-Gompertz_fn(par, length(Y)))^2 * (1:length(Y))^weight
    )
  )
}

Gompertz_fit <-  function(Y, maxCases, weight = 1)
{
  Y1 <- tail(Y,5)
  X1 <- 1:5
  slope <- lm(Y1 ~ X1)$coefficients[2]
  return(
    optim(
      c(Y[1],Y[1] + 2, slope),
      Gompertz_obj,
      Y = Y,
      weight = weight,
      method = "L-BFGS-B",
      lower = c(Y[1] - 1, Y[1] - 1, 0.01),
      upper = c(Y[1] + 1, maxCases, 0.693)
    )$par
  )
}

zeroOne <- function(x)
{
  x <- x - min(x)
  x <- x / max(x)
  return(x)
}

us_state_grid4 <- data.frame(
  row = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8),
  col = c(11, 1, 11, 10, 6, 2, 6, 10, 7, 5, 3, 9, 4, 1, 10, 6, 5, 2, 9, 7, 1, 8, 11, 4, 3, 1, 3, 10, 6, 9, 5, 4, 2, 8, 7, 2, 5, 4, 3, 7, 8, 6, 9, 7, 8, 5, 6, 4, 9, 1, 4),
  code = c("ME", "AK", "NH", "VT", "WI", "ID", "IL", "MA", "MI", "MN", "MT", "NY", "ND", "WA", "CT", "IN", "IA", "NV", "NJ", "OH", "OR", "PA", "RI", "SD", "WY", "CA", "CO", "DE", "KY", "MD", "MO", "NE", "UT", "VA", "WV", "AZ", "AR", "KS", "NM", "NC", "SC", "TN", "DC", "AL", "GA", "LA", "MS", "OK", "FL", "HI", "TX"),
  name = c("Maine", "Alaska", "New Hampshire", "Vermont", "Wisconsin", "Idaho", "Illinois", "Massachusetts", "Michigan", "Minnesota", "Montana", "New York", "North Dakota", "Washington", "Connecticut", "Indiana", "Iowa", "Nevada", "New Jersey", "Ohio", "Oregon", "Pennsylvania", "Rhode Island", "South Dakota", "Wyoming", "California", "Colorado", "Delaware", "Kentucky", "Maryland", "Missouri", "Nebraska", "Utah", "Virginia", "West Virginia", "Arizona", "Arkansas", "Kansas", "New Mexico", "North Carolina", "South Carolina", "Tennessee", "District of Columbia", "Alabama", "Georgia", "Louisiana", "Mississippi", "Oklahoma", "Florida", "Hawaii", "Texas"),
  stringsAsFactors = FALSE
)

plim <- function (x, min, max)
  return(
    pmin(pmax(x, min),max)
  )