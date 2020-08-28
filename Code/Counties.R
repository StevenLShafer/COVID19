# County Summary
newSection("US Counties")


# Counties
plotPred(
  County = c(
    "New York County",
    "Kings County",
    "Bronx County",
    "Queens County",
    "Richmond County"
  ),
  State = "NY",
  Title = "New York City"
)
plotPred(County = "Westchester County", Title = "Westchester County, NY")
plotPred(County = "Bergen County", Title = "Bergen County, NJ")
plotPred(County = "Hudson County", Title = "Hudson County, NJ")
plotPred(County = "King County", State = "WA", Title = "King County (Seattle)")
plotPred(County = "Clark County", State = "WA", Title = "Clark County, Washington")
plotPred(County = "Los Angeles County", State = "CA", Title = "Los Angeles")
plotPred(County = c("Santa Clara County", "San Mateo County", "San Francisco County", "Marin County", "Napa County", "Solano County", "Sonoma County"),
         Title = "Bay Area")
plotPred(County = "San Francisco County", Title = "San Francisco")
plotPred(County = c("Santa Clara County", "San Mateo County"), Title = "Santa Clara and San Mateo")
plotPred(County = "Alameda County", Title = "Alameda County")
plotPred(County = "San Luis Obispo County", Title = "San Luis Obispo, California")
plotPred(County = "Fresno County", Title = "Fresno, California")
plotPred(County = "Santa Barbara County", Title = "Santa Barbara County")
plotPred(County = "Merced County", State = "CA", Title = "Merced County")
plotPred(County = "Yolo County", State = "CA", Title = "Yolo County")
plotPred(County = "San Diego County", State = "CA", Title = "San Diego County")
plotPred(County = "Riverside County", State = "CA", Title = "Riverside County")
plotPred(County = "Imperial County", State = "CA", Title = "Imperial County")
plotPred(County = "Multnomah County", Title = "Multnomah County (Portland)")
plotPred(County = "Utah County", Title = "Utah County")
plotPred(County = "Summit County", State = "UT", Title = "Summit County, Utah")
plotPred(County = "De Soto Parish", Title = "De Soto Parish, Louisiana")
plotPred(County = "St. Francois County", Title = "St. Francois County, Missouri")
plotPred(County = "Dallas County", State = "TX", Title = "Dallas, Texas")
plotPred(County = "Collin County", State = "TX", Title = "Collin Texas")
plotPred(County = "Harris County", State = "TX", Title = "Harris County (Houston)")
plotPred(County = "Tulsa County", State = "OK", Title = "Tulsa County, Oklahoma")
plotPred(County = "McLean County", State = "IL", Title = "McLean County, Illinois")
plotPred(County = "Cook County", State = "IL", Title = "Cook County, Illinois")
plotPred(County = "Suffolk County", State = "MA", Title = "Suffolk County (Boston)")
plotPred(County = "Polk County", State = "IA", Title = "Polk County, Iowa")
plotPred(County = "Johnson County", State = "IA", Title = "Johnson County, Iowa")
plotPred(County = "Meade County", State = "SD", Title = "Meade County, South Dakota (Sturgis)")
plotPred(County = "Erie County", State = "NY", Title = "Erie County, New York")
plotPred(County = "Oakland County", State = "MI", Title = "Oakland County, Michigan")
plotPred(County = "Washtenaw County", State = "MI", Title = "Washtenaw County, Michigan (Ann Arbor)")
plotPred(County = "Wayne County", State = "MI", Title = "Wayne County (Detroit)")
plotPred(County = "City of St. Louis", Title = "St. Louis (City)")
plotPred(County = "St. Louis County", Title = "St. Louis (County)")
plotPred(County = "Baltimore City", Title = "Baltimore (City)")
plotPred(County = "Durham County", Title = "Durham County")
plotPred(County = "Cuyahoga County", Title = "Cuyahoga County (Cleveland)")
plotPred(County = "Miami-Dade County", Title = "Miami-Dade")
plotPred(County = "Maricopa County", Title = "Maricopa County")
plotPred(County = "Denver County", State = "CO", Title = "Denver County")
plotPred(County = "Arapahoe County", State = "CO", Title = "Arapahoe County")
plotPred(County = "Montrose County", State = "CO", Title = "Montrose County")
print(pptx, target = pptxfileName)
shell.exec(pptxfileName)
