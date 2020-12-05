# For Saudi Talk
pptx <- read_pptx(paste0(dirSheets, "Template.blank.pptx"))
X <- plotPred(Country = "Saudi Arabia")
CASES <- X$results$CASES
plotPred(Country = "Saudi Arabia")
plotMortality(Country = "Saudi Arabia")
ggplot(CASES,aes(x=Date, y=Delta)) +
  geom_point()
