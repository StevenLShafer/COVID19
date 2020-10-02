# International
newSection("International")
# Western Europe
plotPred(Country = "Italy", Title = "Italy")
plotPred(Country = "Spain", Title = "Spain")
plotPred(Country = "Portugal", Title = "Portugal")
plotPred(Country = "France", Title = "France")
plotPred(Country = "Germany", Title = "Germany")
plotPred(Country = "Switzerland", Title = "Switzerland")
plotPred(Country = "Austria", Title = "Austria")
plotPred(Country = "Netherlands", Title = "Netherlands")
plotPred(Country = "United Kingdom", Title = "United Kingdom")
plotPred(Country = "Greece", Title = "Greece")
plotPred(Country = "Luxembourg", Title = "Luxembourg")
plotPred(Country = "Belgium", Title = "Belgium")


plotPred(Country = "Sweden", Title = "Sweden")
plotPred(Country = "Norway", Title = "Norway")
plotPred(Country = "Denmark", Title = "Denmark")
plotPred(Country = "Finland", Title = "Finland")

plotPred(Country = "Ireland", Title = "Ireland")
plotPred(Country = "Iceland", Title = "Iceland")
# plotPred(Country = "Rwanda", Title = "Rwanda"

plotPred(Country = "Australia", Title = "Australia")
plotPred(Country = "New Zealand", Title = "New Zealand")

# Asia
plotPred(Country = "China", Title = "China")
plotPred(Country = "Taiwan")
plotPred(Country = "Vietnam")
plotPred(Country = "Thailand")
plotPred(Country = "South Korea", Title = "South Korea")
plotPred(Country = "Singapore", Title = "Singapore")
plotPred(Country = "Japan", Title = "Japan")

# Africa
plotPred(Country = "South Africa", Title = "South Africa")

# Middle East
plotPred(Country = "Israel", Title = "Israel")
plotPred(Country = "Turkey", Title = "Turkey")
plotPred(Country = "Lebanon")
plotPred(Country = "Iran", Title = "Iran")
plotPred(Country = "Iraq")
plotPred(Country = "Egypt", Title = "Egypt")
plotPred(Country = "Saudi Arabia", Title = "Saudi Arabia")
plotPred(Country = "Kuwait")
plotPred(Country = "United Arab Emirates")
plotPred(Country = "Oman")
plotPred(Country = "Qatar")
plotPred(Country = "Bahrain", Title = "Bahrain")

# South America
plotPred(
  Country = c(
    "Argentina", 
    "Bolivia", 
    "Brazil",
    "Chile",
    "Colombia", 
    "Ecuador", 
    "French Guiana",
    "Guyana", 
    "Paraguay", 
    "Peru", 
    "Suriname", 
    "Uruguay", 
    "Venezuela"
    ),
  Title = "South America",
  Subtitle = "Argentina, Bolivia, Brazil, Chile, Colombia, Ecuador, French Guiana, Guyana, Paraguay, Peru, Suriname, Uruguay, and Venezuela"
)

plotPred(Country = "Argentina")
plotPred(Country = "Bolivia")
plotPred(Country = "Brazil", Title = "Brazil")
plotPred(Country = "Chile")
plotPred(Country = "Colombia")
plotPred(Country = "Ecuador")
plotPred(Country = "French Guiana")
plotPred(Country = "Guyana")
plotPred(Country = "Paraguay")
plotPred(Country = "Peru")
plotPred(Country = "Uruguay")
plotPred(Country = "Venezuela")

# Central America
plotPred(
  Country = c(
    "El Salvador", 
    "Costa Rica", 
    "Belize", 
    "Guatemala", 
    "Honduras", 
    "Nicaragua",
    "Panama"
  ),
  Title = "Central America",
  Subtitle = "El Salvador, Costa Rica, Belize, Guatemala, Honduras, Nicaragua, and Panama"
)
plotPred(Country = "El Salvador")
plotPred(Country = "Costa Rica")
plotPred(Country = "Belize")
plotPred(Country =  "Guatemala")
plotPred(Country = "Honduras")
plotPred(Country =  "Nicaragua")
plotPred(Country = "Panama")
plotPred(Country = "Mexico", Title = "Mexico")

plotPred(Country= "Canada")

# Asia
plotPred(Country = "Ukraine")
plotPred(Country = "Russia", Title = "Russia")
plotPred(Country = "India", Title = "India")
plotPred(Country = "Pakistan", Title = "Pakistan")
print(pptx, target = pptxfileName)
#shell.exec(pptxfileName)
