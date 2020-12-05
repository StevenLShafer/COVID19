# Canadian Provinces

newSection("Canada")
plotPred(Country = "Canada")
for (i in 1:nrow(Provinces_Canada))
{
  plotPred(
    State = Provinces_Canada$Province[i], 
    Country = "Canada", 
    Title = paste0(Provinces_Canada$Province[i], ", Canada,")
  )
}
print(pptx, target = pptxfileName)
#shell.exec(pptxfileName)

