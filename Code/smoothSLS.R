smoothSLS <- function(X, RespectLow = TRUE)
{
  X <- c(rep(0,10), 1, rep(0,10))
  plot(1:length(X), X)
  X1 <- rollmean(X, 3, fill =list(mean(head(X,3)), NA, mean(tail(X,3))))
  lines(1:length(X), X1)
  X2 <- rollmean(X1, 3, fill =list(mean(head(X1,3)), NA, mean(tail(X1,3))))
  lines(1:length(X), X2)
  X3 <- rollmean(X2, 3, fill =list(mean(head(X2,3)), NA, mean(tail(X2,3))))
  lines(1:length(X), X3)
  X4 <- X3
  if (RespectLow)
  {
    for (i in 9:length(X)) 
      if (sum(X[(i-8):i]) < 5)
        X4[(i-8):i] <- X[(i-8):i]
  }
  return(X4)
}
