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
plotPred(County = c(
  "Santa Clara County", 
  "San Mateo County", 
  "San Francisco County", 
  "Marin County", 
  "Napa County", 
  "Solano County", 
  "Sonoma County"
  ),
  Title = "Bay Area",
  Subtitle = "Marin, Napa, San Mateo, San Francisco, Santa Clara, Solano, and Sonoma Counties")
plotPred(County = "Marin County", Title = "Marin County, California")
plotPred(County = "Napa County", Title = "Napa County, California")
plotPred(County = "San Francisco County", Title = "San Francisco")
plotPred(County = "San Mateo County", Title = "San Mateo County, California")
plotPred(County = "Santa Clara County", Title = "Santa Clara County, California")
plotPred(County = c("Santa Clara County", "San Mateo County"), 
         Title = "Santa Clara and San Mateo Counties")

plotPred(County = "Alameda County", Title = "Alameda County, California")
plotPred(County = "San Luis Obispo County", Title = "San Luis Obispo, California")
plotPred(County = "Fresno County", Title = "Fresno, California")
plotPred(County = "Santa Barbara County", Title = "Santa Barbara County, California")
plotPred(County = "Merced County", State = "CA", Title = "Merced County, California")
plotPred(County = "Yolo County", State = "CA", Title = "Yolo County, California")
plotPred(County = "San Diego County", State = "CA", Title = "San Diego County")
plotPred(County = "Riverside County", State = "CA", Title = "Riverside County")
plotPred(County = "Imperial County", State = "CA", Title = "Imperial County")
plotPred(County = "Multnomah County", Title = "Multnomah County (Portland)")
plotPred(County = "Utah County", Title = "Utah County")
plotPred(County = "Summit County", State = "UT", Title = "Summit County, Utah")
plotPred(County = "De Soto Parish", Title = "De Soto Parish, Louisiana")
plotPred(County = "St. Francois County", Title = "St. Francois County, Missouri")
plotPred(County = "Howard County", State = "MO", Title = "Howard County, Missouri")
plotPred(County = "Cooper County", State = "MO", Title = "Cooper County, Missouri")
plotPred(County = "Trousdale County", Title = "Trousdale County, Tennessee")
# Texas
plotPred(County = "Harris County", State = "TX", Title = "Harris County (Houston)")
plotPred(County = "Dallas County", State = "TX", Title = "Dallas, Texas")
plotPred(County = "Tarrant County", State = "TX", Title = "Tarrant County (Fort Worth)")
plotPred(County = "Collin County", State = "TX", Title = "Collin County, Texas")
plotPred(County = "Bexar County", State = "TX", Title = "Bexar County (San Antonio)")
plotPred(County = "Hidalgo County", State = "TX", Title = "Hidalgo County, Texas")
plotPred(County = "Travis County", State = "TX", Title = "Travis County (Austin)")
plotPred(County = "El Paso County", State = "TX", Title = "El Paso County, Texas")
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
plotPred(County = "Durham County", Title = "Durham County, North Carolina")
plotPred(County = "Arlington County", State = "VA", Title = "Arlington County, Virginia")
plotPred(County = "Cuyahoga County", Title = "Cuyahoga County (Cleveland)")
plotPred(County = "Miami-Dade County", Title = "Miami-Dade")
plotPred(County = "Maricopa County", Title = "Maricopa County, Arizona")
plotPred(County = "Denver County", State = "CO", Title = "Denver County")
plotPred(County = "Arapahoe County", State = "CO", Title = "Arapahoe County, Colorado")
plotPred(County = "Montrose County", State = "CO", Title = "Montrose County, Colorado")
plotPred(County = "Androscoggin County", State = "ME", Title = "Androscoggin County, Maine")
plotPred(County = "Park County", State = "WY")
print(pptx, target = pptxfileName)
#shell.exec(pptxfileName)
