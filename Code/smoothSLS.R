smoothSLS <- function(X, RespectLow = TRUE)
{
  X1 <- rollmean(X, 5, fill =list(mean(head(X,5)), NA, mean(tail(X,5))))
  X2 <- rollmean(X1, 5, fill =list(mean(head(X1,5)), NA, mean(tail(X1,5))))
 # X3 <- rollmean(X2, 5, fill =list(mean(head(X2,5)), NA, mean(tail(X2,5))))
  X4 <- X2
  if (RespectLow)
  {
    for (i in 9:length(X)) 
      if (sum(X[(i-8):i]) < 5)
        X4[(i-8):i] <- X[(i-8):i]
  }
  return(X4)
}
