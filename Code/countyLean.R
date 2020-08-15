# Determine 2016 election lean by county

# MIT Election Data and Science Lab, 2018, "countypres_2000-2016.tab", 
# County Presidential Election Returns 2000-2016, 
# https://doi.org/10.7910/DVN/VOQCHQ/HEIJCQ, Harvard Dataverse, V6, UNF:6:ZZe1xuZ5H2l4NUiSRcRf8Q== [fileUNF]

setwd("g:/projects/covid")
DATA <- read.csv("countypres_2000-2016.MIT ElectionData Science Lab.csv", stringsAsFactors = FALSE)
DATA <- DATA[DATA$year == 2016,]
DATA <- DATA[!is.na(DATA$party),]

countyLean <- data.frame(
  FIPS = unique(DATA$FIPS),
  Lean = 0
)
Republican <- DATA[DATA$party == "republican",]
Democrat   <- DATA[DATA$party == "democrat",]

for (i in 1:nrow(countyLean))
{
  R <- Republican$candidatevotes[Republican$FIPS == countyLean$FIPS[i]]
  if (length(R) == 0) R <- 0
  D <- Democrat$candidatevotes[Democrat$FIPS == countyLean$FIPS[i]]
  if (length(D) == 0) D <- 0
  countyLean$Lean[i] <- R/(R+D)
}

write.csv(countyLean, "County Lean.csv", row.names = FALSE)
