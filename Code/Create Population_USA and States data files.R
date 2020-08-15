# Create Population_USA.csv from 2019 Census Estimation

SOURCE <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"
Population_USA <- read.csv(SOURCE)[, c("STATE", "COUNTY", "STNAME", "CTYNAME", "POPESTIMATE2019")]
names(Population_USA) <- c("codeState", "codeCounty", "State", "County", "Population")

Cases_USA <- read.csv("g:/projects/COVID/2020-05-31/Cases_USA.2020-05-31.csv")
# Notes
# Create States database (FIPS, Abbreviation, Full)
States <- data.frame(
  FIPS = unique(Cases_USA$stateFIPS)
)
States$Abbreviation <- Cases_USA$State[match(States$FIPS, Cases_USA$stateFIPS)]
States$State <- Population_USA$State[match(States$FIPS, Population_USA$codeState)]

# Countys with a Code of 0 match the entire state. Will set population to 0, to match unallocated cases in
# Cases_USE and Deaths_USA
Population_USA$Population[Population_USA$codeCounty == 0] <- 0

# The county FIPS in the usafacts data is the state * 1000 + the county. Need to add this to the
# county population data
Population_USA$CountyFIPS <- Population_USA$codeState * 1000 + Population_USA$codeCounty
Population_USA <- Population_USA[,c("CountyFIPS","County", "State", "Population")]

Population_USA <- Population_USA[order(Population_USA$CountyFIPS),]

if (sum(Cases_USA$CountyFips != Population_USA$CountyFIPS) > 0)
  cat("Matching failed between allCases and Population\n")

setwd("g:/Projects/COVID")
write.csv(Population_USA, "Population_USA.csv",row.names = FALSE)
write.csv(States, "States.csv", row.names = FALSE)


SOURCE <- "https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt"
Counties <- read.csv(SOURCE, stringsAsFactors = FALSE)
Counties$FIPS <- Counties$STATEFP * 1000 + Counties$COUNTYFP
Counties <- Counties[,c("LONGITUDE", "LATITUDE", "FIPS")]
Counties <- suppressWarnings(
  usmap_transform(Counties)
)
write.csv(Counties, "Counties.csv", row.names = FALSE)
