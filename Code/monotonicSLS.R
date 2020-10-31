# Monotonic function: force the function to only increase. Decreases are spread over prior totals
forceMonotonic <- function(X)
{
  first <- which(names(X) == "X1.22.20")
  last <- ncol(X)
  
  for (i in 1:nrow(X))
  {
    cat(i, " ")
    X[i, first:last] <- monotonicSLS(X[i, first:last])
  }
  return(X)
}

monotonicSLS <- function (X)
{
  for (i in length(X):2)
  {
    if (is.na(X[i])) X[i] <- X[i-1]
    if (X[i-1] > X[i])
    {
      cat("fixing at i = ", i, "\n")
      fraction <- X[i] / X [i-1]
      for (j in 1:(i-1))
        X[j] <- X[j] * fraction
    }
  }
  return(X)
}
