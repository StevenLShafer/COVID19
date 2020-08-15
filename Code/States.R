# Summary by state
# Summary for all 50 states
newSection("US States")
for (i in 1:nrow(States))
  plotPred(State = States$Abbreviation[i], Title = States$State[i])
print(pptx, target = pptxfileName)
shell.exec(pptxfileName)
