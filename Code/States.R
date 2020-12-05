# Summary by state
# Summary for all 50 states
newSection("US States")
States$addPlot <- FALSE
States$addPlot[States$Abbreviation == "CA"] <- TRUE
for (i in 1:nrow(States))
{
  plotPred(State = States$Abbreviation[i], Title = States$State[i], addPlot = States$addPlot[i])
}
print(pptx, target = pptxfileName)
#shell.exec(pptxfileName)

