# library(curl)
# library(RPushbullet)

# Directory <- "g:/projects/COVID"
# setwd(Directory)
# today <- Sys.Date()
# todayText <- as.character(today)
# yesterdayText <- format(today - 1, "X%m.%e.%y")
# yesterdayText <- gsub("X0","X",yesterdayText)
# yesterdayText <- gsub(" ","",yesterdayText)

if (!dir.exists(dirTodayUpdate)) dir.create(dirTodayUpdate)
if (!dir.exists(dirTodayUpdateData)) dir.create(dirTodayUpdateData)

fileDownload <- function(
  source,
  name,
  extension = ".csv",
  persist = TRUE,
  checkLastColumn = FALSE,
  forceMonotonicFlag = FALSE
)
{
  # source <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  # name <- "Deaths_Global"
  # extension <- ".csv"
  # persist <- TRUE
  # checkLastColumn <- FALSE
  # forceMonotonic <- TRUE

  filename <- paste0(dirTodayUpdateData,name,".", todayText, ".raw", extension)
  if (file.exists(filename)) cat(name, "already downloaded.\n")
  while (!file.exists(filename))
  {
    if (persist == TRUE)
    {
      Head <- curlGetHeaders(source)
      Date <- Head[grep("^Date:", Head)]
      # cat(paste0("Date filed in header of ", name, ": ", Date), "\n")
      Date <- substr(Date,12,22)
      Date <- as.Date(Date,"%d %b %Y")
      Date <- sort(Date, decreasing = TRUE)[1]
      cat(paste0("Captured Date of ", name, ": ", Date), "\n")
      # Add ETAG Check for new contents
      if (Date >= today)
      {
        cat("Downloading to file: ", filename, "\n")
        curl_download(source, filename)
        if (checkLastColumn)
        {
          x <- unlist(strsplit(readLines(filename, n=1), ","))
          cat(paste0("Last Column of ", name, ": ", tail(x, 1)), "\n")
          col <- grep(yesterday, as.Date(tail(x,3),"%m/%d/%y"))
          if (as.Date(tail(x,1), "%m/%d/%y") != yesterday)
            {
            cat("Removing file ", filename, "\n")
            file.remove(filename)
            Sys.sleep(60)
          } else {
            cat("File OK: ", filename, "\n")
            }
        }
      } else {
        cat("Awaiting today's file\n")
        Sys.sleep(30)
      }
    } else {
      cat("Downloading but not checking ", filename, "\n")
      curl_download(source, filename)
    }
  }
  if (forceMonotonicFlag)
  {
    X <- forceMonotonic(read.csv(filename))
    write.csv(X, filename, row.names=FALSE)
    cat("\n")
  }
}

# US Data  ******************************************************************************
fileDownload(
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
#  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv",
  "Cases_USA",
  checkLastColumn = TRUE,
  forceMonotonicFlag = TRUE
)
fileDownload(
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv",
# "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv",
  "Deaths_USA",
  checkLastColumn = TRUE,
  forceMonotonicFlag = TRUE
)
fileDownload(
  "https://covidtracking.com/api/v1/states/daily.csv",
#  "https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv",
  "Testing_USA"
)

# Global Data  ************************************************************************
fileDownload(
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  "Cases_Global",
  forceMonotonicFlag = TRUE
)
fileDownload(
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
  "Deaths_Global",
  forceMonotonicFlag = TRUE
)
fileDownload(
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
  "Testing_Global"
)

# Mobility Data ***********************************************************************
# fileDownload(
#   "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
#   "Mobility",
#   persist = TRUE
# )

# Ensemble prediction *****************************************************************
SOURCE <- "https://github.com/reichlab/covid19-forecast-hub/raw/master/data-processed/COVIDhub-ensemble/2020-07-06-COVIDhub-ensemble.csv"
# Change to most recent Monday
SOURCE1 <- gsub("2020-07-06", today - as.POSIXlt(today)$wday + 1, SOURCE)
Head <- curlGetHeaders(SOURCE1)
if (attr(Head, "status") == 404)
{
  cat("Ensemble predications are not posted yet, going back 1 week\n")
  SOURCE1 <- gsub("2020-07-06", today - 7 - as.POSIXlt(today)$wday + 1, SOURCE)
}
fileDownload(
  SOURCE1,
  "Ensemble",
  persist = TRUE
)

cat("All files downloaded.\n")
pbPost(
#  devices = "Phone",
  channel = "",
  type = "note",
  title = "Message from R",
  body = "The most recent COVID files have been downloaded.",
  apikey = "o.Jb1UN5cEOOZnaZ7Tp3rsxf4vShe82xXy"
  )
