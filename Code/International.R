# International
newSection("International")
plotPred(Country = "Italy", Title = "Italy")
plotPred(Country = "Spain", Title = "Spain")
plotPred(Country = "Portugal", Title = "Portugal")
plotPred(Country = "France", Title = "France")
plotPred(Country = "Germany", Title = "Germany")
plotPred(Country = "Switzerland", Title = "Switzerland")
plotPred(Country = "Austria", Title = "Austria")
plotPred(Country = "Sweden", Title = "Sweden")
plotPred(Country = "Netherlands", Title = "Netherlands")
plotPred(Country = "England", Title = "United Kingdom")
plotPred(Country = "Iceland", Title = "Iceland")
# plotPred(Country = "Paraguay", Title = "Paraguay")
# plotPred(Country = "Rwanda", Title = "Rwanda"
plotPred(Country = "Canada", Title = "Canada")
plotPred(Country = "Australia", Title = "Australia")
plotPred(Country = "New Zealand", Title = "New Zealand")
plotPred(Country = "China", Title = "China")
plotPred(Country = "Taiwan")

plotPred(Country = "South Korea", Title = "South Korea")
plotPred(Country = "Singapore", Title = "Singapore")
plotPred(Country = "Japan", Title = "Japan")
plotPred(Country = "South Africa", Title = "South Africa")
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
    "Guyana", 
    "Paraguay", 
    "Peru", 
    "Suriname", 
    "Uruguay", 
    "Venezuela"
    ),
  Title = "South America",
  Subtitle = "Argentina, Bolivia, Brazil, Chile, Colombia, Ecuador, Guyana, Paraguay, Peru, Suriname, Uruguay, and Venezuela"
)

plotPred(Country = "Argentina")
plotPred(Country = "Bolivia")
plotPred(Country = "Brazil", Title = "Brazil")
plotPred(Country = "Chile")
plotPred(Country = "Colombia")
plotPred(Country = "Ecuador")
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
plotPred(Country = "Ukraine")
plotPred(Country = "Russia", Title = "Russia")
plotPred(Country = "India", Title = "India")
plotPred(Country = "Pakistan", Title = "Pakistan")
print(pptx, target = pptxfileName)
shell.exec(pptxfileName)
