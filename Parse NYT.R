require(rvest)
require(jsonlite)

X <- readLines(  "https://www.nytimes.com/interactive/2020/us/covid-college-cases-tracker.html?action=click&module=Top%20Stories&pgtype=Homepage")
X1 <- X[grep("NYTG_schools",X)]
X2 <- substr(X1,22,10000000)
X3 <- gsub("];","]", X2)
str_count(X1, "];")
DATA <- fromJSON(X3)
DATA$logo <- NULL
DATA$label <- NULL
DATA$medicalnote <- NULL
DATA$location <- NULL
DATA$coord <- NULL


head(DATA)
