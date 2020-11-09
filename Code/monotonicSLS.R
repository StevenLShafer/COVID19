# Monotonic function: force the function to only increase. Decreases are spread over prior totals
forceMonotonic <- function(X)
{
  first <- which(names(X) == "X1.22.20")
  last <- ncol(X)
  
  for (i in 1:nrow(X))
  {
    cat(i, "\n")
    X[i, first:last] <- monotonicSLS(X[i, first:last])
  }
  return(X)
}

monotonicSLS <- function (X)
{
  NAs <- which(is.na(X))
  if (length(NAs) > 0) X[NAs] <- X[NAs-1]
  for (i in length(X):2)
  {
    if (X[i-1] > X[i])
    {
      cat("   fixing at i = ", i, "\n")
      X[1:(i-1)] <- sapply(X[1:(i-1)], function(x) X[i] / X [i-1] * x)
      # for (j in 1:(i-1))
      #   X[j] <- X[j] * fraction
    }
  }
  return(X)
}


# Vectorized code
# Replace NAs with prior entry
# X <- 1:10
# X[5] <- NA
# X
# X
# 
# #Multiply all entries before 5 by Factor
# fraction <- 0.8
# sapply(X[1:5], function(x) fraction * x)

