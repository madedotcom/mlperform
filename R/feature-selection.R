# Feature selection

library(data.table)

buildModel <- function(model.formula, training){
 model <- lm(model.formula, training)
 invisible(model)
}

forwardFeatureSelection <- function(model.features, training, test){

 selected.features <- character()
 selected.metric <- numeric()
 unselected.features <- model.features

 for (i in 1:length(model.features)) {
   print(paste0("Iteration #:", i))

   metrics <- data.table("feature" = unselected.features)
   metrics[, metric := numeric()]

   for (j in 1:length(unselected.features))
   {
     cur.feature <- unselected.features[j]
     print(paste0("Try feature:", cur.feature))
     current.features <- c(selected.features, cur.feature)

     # Build model
     model.formula <- as.formula(paste("isConverted"," ~ ",paste(current.features, collapse = "+")))
     print(paste0("Model formula:", conversion.model.formula))
     model <- buildModel(model.formula, training)

     # Test model
     predictions <- predict(model.conversion, test)
     cur.metric <- calculateMetric()
     metrics[feature == cur.feature, metric := cur.metric]
     print(paste0("Metric value: ", cur.metric))
   }
   print(paste0("Iteration #:", i, " results:"))
   print(metrics)
   best.feature <- metrics[which.max(metric), feature]
   best.fscore <- metrics[which.max(metric), fscore]
   print(paste0("Selected feature = ", best.feature))
   print(paste0("F-score: ", best.fscore))
   selected.features <- c(selected.features, best.feature)
   selected.fscore <- c(selected.fscore, best.fscore)
   unselected.features <- unselected.features[unselected.features != best.feature]
 }
 results <- data.frame("feature" = selected.features, "fscore" = selected.fscore)

}
