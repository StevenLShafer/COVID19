assignSign <- function(X)
{
  X1 <- rep("No Change (-1% to +1%)", length(X))
  X1 <- "No Change (-1% to +1%)"
  X1[X < -3] <- "Decreasing > -3%"
  X1[X >= -3 & X < -1] <- "Decreasing between -1% and -3%"
  X1[X <= 3 & X > 1] <- "Increasing between +1% and +3%"
  X1[X > 3] <- "Increasing > +3%"
  X1 <- factor(
    X1,
    levels = c(
      "Increasing > +3%",
      "Increasing between +1% and +3%",
      "No Change (-1% to +1%)",
      "Decreasing between -1% and -3%",
      "Decreasing > -3%")
  )
}