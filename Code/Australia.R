# Australian States

newSection("Australia")
plotPred(Country = "Australia")
for (i in 1:nrow(States_Australia))
{
  plotPred(
    State = States_Australia$State[i], 
    Country = "Australia", 
    Title = paste0(States_Australia$State[i], ", Australia,")
  )
}
print(pptx, target = pptxfileName)
#shell.exec(pptxfileName)

