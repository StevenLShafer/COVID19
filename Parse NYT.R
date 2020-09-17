require(rvest)
require(jsonlite)
X <- readLines(  "https://www.nytimes.com/interactive/2020/us/covid-college-cases-tracker.html?action=click&module=Top%20Stories&pgtype=Homepage")
X <- grep("NYTG_schools",X)
fromJSON(as.character(X), flatten=TRUE)

X <- read_html(
  "https://www.nytimes.com/interactive/2020/us/covid-college-cases-tracker.html?action=click&module=Top%20Stories&pgtype=Homepage")

X <- html_session(
  "https://www.nytimes.com/interactive/2020/us/covid-college-cases-tracker.html?action=click&module=Top%20Stories&pgtype=Homepage")

xml_structure(X)
html_nodes(X, "#collegeMap")

html_nodes(X, ".g-popup")
xml_structure(X1)
html_nodes(X2, "div")


head(xml_structure(X1))
X2 <- html_nodes(X1, "main")
X3 <- html_nodes(X2, "article")
X4 <- html_nodes(X3, "section")
X5 <- html_nodes(X4, "div")
X6 <- html_nodes(X5[1], "div")
X7 <- html_nodes(X6[1], "div")
X8 <- html_nodes(X7[grep("g-asset g-graphic", X7)[1]], "div")
X9 <- html_nodes(X8[grep("mapcontainer", X8)[1]], "div")
X10 <- html_nodes(X9[grep("collegeMap", X9)[1]], "div")
X11 <- html_nodes(X10, "div")




html_nodes(X, "school-map-label")

doc <- paste("<html>",
             "<body>",
             "<span class='a1 b1'> text1 </span>",
             "<span class='b1'> text2 </span>",
             "</body>",
             "</html>"
)
read_html(doc) %>% html_nodes(".b1")


