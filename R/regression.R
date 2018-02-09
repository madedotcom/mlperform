RootMSE <- function(v1, v2){
  return(round(sqrt(mean((v1 - v2)^2)),2))
}

Rsquared <- function(predicted, actual){
  actualn <- as.numeric(actual)
  return(round(1 - sum((predicted - actualn)^2)/sum((actualn - mean(actualn))^2),3))
}

Radjsquared <- function(predicted, actual, N, nvar){
  R2adjusted <- 1 - (sum((predicted - actual)^2)/(N-nvar-1))/(sum((actual - mean(actual))^2)/(N-1))
  return(round(R2adjusted,3))
}

# MAPE: https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
MeanAbsolutePercentageError <- function(predicted, actual)
{
  res <- abs(predicted-actual)/actual
  res[is.infinite(res)] <- NA
  n <- sum(!is.na(res))
  res <- 100*sum(res, na.rm = TRUE)/n
  return (paste(round(res,2), '%', sep=""))
}

# Median APE
MedianAbsolutePercentageError <- function(predicted, actual)
{
  res <- abs(predicted-actual)/actual
  res[is.infinite(res)] <- NA
  res <- 100*median(res, na.rm = TRUE)
  return (paste(round(res,2), '%', sep=""))
}

# SMAPE: https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error
SymmetricMeanAbsolutePercentageError <- function(predicted, actual)
{
  res <- abs(predicted-actual)/(abs(actual)+abs(predicted))
  res[is.infinite(res)] <- NA
  n <- sum(!is.na(res))
  res <- 200*sum(res, na.rm = TRUE)/n
  return (paste(round(res,2), '%', sep=""))
}

# APE: https://blog.arkieva.com/two-sides-of-the-mape-coin/
AbsolutePercentageError <- function(predicted, actual)
{
  res1 <- sum(abs(predicted-actual))
  res2 <- sum(actual)
  res <- 100*res1/res2
  return (paste(round(res,2), '%', sep=""))
}
