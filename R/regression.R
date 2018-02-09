#' Calculates root mean square error
#'
#' @export
#' @param v1 first vector of values
#' @param v2 second vector of values
#' @return root mean square error
RootMSE <- function(v1, v2){
  return(round(sqrt(mean((v1 - v2)^2)),2))
}

#' Calculates R-squared metric
#'
#' @export
#' @param predicted Predicted values
#' @param actual Actual values
#' @return R-squared metric
Rsquared <- function(predicted, actual){
  predictedn <- as.numeric(predicted)
  actualn <- as.numeric(actual)
  return(round(1 - sum((predictedn - actualn)^2)/sum((actualn - mean(actualn))^2),3))
}

#' Calculates adjusted R-squared metric
#'
#' @export
#' @param predicted Predicted values
#' @param actual Actual values
#' @return Adjusted R-squared metric
Radjsquared <- function(predicted, actual, N, nvar){
  R2adjusted <- 1 - (sum((predicted - actual)^2)/(N-nvar-1))/(sum((actual - mean(actual))^2)/(N-1))
  return(round(R2adjusted,3))
}

#' Calculates MAPE: https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
#'
#' @export
#' @param predicted Predicted values
#' @param actual Actual values
#' @return MAPE
MeanAbsolutePercentageError <- function(predicted, actual)
{
  res <- abs(predicted-actual)/actual
  res[is.infinite(res)] <- NA
  n <- sum(!is.na(res))
  res <- 100*sum(res, na.rm = TRUE)/n
  return (paste(round(res,2), '%', sep=""))
}

#' Calculates median APE
#'
#' @export
#' @param predicted Predicted values
#' @param actual Actual values
#' @return Median APE
MedianAbsolutePercentageError <- function(predicted, actual)
{
  res <- abs(predicted-actual)/actual
  res[is.infinite(res)] <- NA
  res <- 100*median(res, na.rm = TRUE)
  return (paste(round(res,2), '%', sep=""))
}

#' Calculates SMAPE: https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error
#'
#' @export
#' @param predicted Predicted values
#' @param actual Actual values
#' @return SMAPE
SymmetricMeanAbsolutePercentageError <- function(predicted, actual)
{
  res <- abs(predicted-actual)/(abs(actual)+abs(predicted))
  res[is.infinite(res)] <- NA
  n <- sum(!is.na(res))
  res <- 200*sum(res, na.rm = TRUE)/n
  return (paste(round(res,2), '%', sep=""))
}

#' Calculates APE: https://blog.arkieva.com/two-sides-of-the-mape-coin/
#'
#' @export
#' @param predicted Predicted values
#' @param actual Actual values
#' @return APE
AbsolutePercentageError <- function(predicted, actual)
{
  res1 <- sum(abs(predicted-actual))
  res2 <- sum(actual)
  res <- 100*res1/res2
  return (paste(round(res,2), '%', sep=""))
}
