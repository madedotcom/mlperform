#' Calculates classification accuracy
#'
#' @export
#' @param tp True positives
#' @param tn True negatives
#' @param fp False positives
#' @param fn False negatives
#' @return Classification accuracy
metricAccuracy <- function(tp, tn, fp, fn) {
  Accuracy <- (tp + tn) / (tp + tn + fp + fn)
  return(Accuracy)
}

#' Calculates classification sensitivity
#'
#' @export
#' @param tp True positives
#' @param tn True negatives
#' @param fp False positives
#' @param fn False negatives
#' @return Classification sensitivity
metricSensitivity <- function(tp, tn, fp, fn) {
  Sensitivity <- tp / (tp + fn)
  return(Sensitivity)
}

#' Calculates classification precision
#'
#' @export
#' @param tp True positives
#' @param tn True negatives
#' @param fp False positives
#' @param fn False negatives
#' @return Classification precision
metricPrecision <- function(tp, tn, fp, fn) {
  Precision <- tp / (tp + fp)
  return(Precision)
}

#' Calculates classification F-score
#'
#' @export
#' @param tp True positives
#' @param tn True negatives
#' @param fp False positives
#' @param fn False negatives
#' @return Classification F-score
metricFscore <- function(tp, tn, fp, fn) {
  Sensitivity <- metricSensitivity(tp, tn, fp, fn)
  Precision <- metricPrecision(tp, tn, fp, fn)
  Fscore <- 2 * Sensitivity * Precision / (Sensitivity + Precision)
  return(Fscore)
}

#' Calculates Kappa metric for classification
#'
#' @export
#' @param tp True positives
#' @param tn True negatives
#' @param fp False positives
#' @param fn False negatives
#' @return Kappa metric
metricKappa <- function(tp, tn, fp, fn) {
  ma <- ((tp+fp)*(tp+fn))/(tp+tn+fp+fn)
  mb <- ((tn+fp)*(tn+fn))/(tp+tn+fp+fn)
  pe <- (ma + mb) / (tp+tn+fp+fn)
  Accuracy <- metricAccuracy(tp, tn, fp, fn)
  Kappa <- (Accuracy - pe) / (1 - pe)
  return(Kappa)
}

#' Calculates various classification metrics
#'
#' @export
#' @param tp True positives
#' @param tn True negatives
#' @param fp False positives
#' @param fn False negatives
#' @return data frame with classification metrics
evaluateClassification <- function(tp, tn, fp, fn){

  Accuracy <- metricAccuracy(tp, tn, fp, fn)
  Sensitivity <- metricSensitivity(tp, tn, fp, fn)
  Precision <- metricPrecision(tp, tn, fp, fn)
  Fscore <- metricFscore(tp, tn, fp, fn)
  Kappa <- metricKappa(tp, tn, fp, fn)

  ret <- data.frame("FSCORE" = Fscore,
                    "SENSITIVITY" = Sensitivity,
                    "PRECISION" = Precision,
                    "ACCURACY" = Accuracy,
                    "KAPPA" = Kappa)
  return(ret)
}
