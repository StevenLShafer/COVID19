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

assignHeadroom <- function(X)
{
  X1 <- rep("None", length(X))
  X1[X > 0   & X < 5] <- "Less than 5%"
  X1[X >= 5  & X < 10] <- "5% to 10%"
  X1[X >= 10 & X < 20] <- "10% to 20%"
  X1[X >= 20] <- "More than 20%"
  X1 <- factor(
    X1,
    levels = c(
      "None",
      "Less than 5%",
      "5% to 10%",
      "10% to 20%",
      "More than 20%")
  )
}