library(ROCR)

auc <- function(y, predicted) {
  pred <- prediction(ifelse(predicted == "Yes",1,0), ifelse(y == "Yes",1,0))
  perf <- performance(pred, measure = "auc")
  return(attr(perf, "y.values")[[1]])
}