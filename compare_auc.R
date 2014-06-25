source("functions.R")
source("extract_donations.R")

rm(list=c("donations.data"))
gc(TRUE)

nb.days <- 350

shrinkage.eval <- 0.1
n.trees.eval <- 300


at_least_1_teacher_referred_donor.eval <- make.gbm.train.model.estimate(
  variable="at_least_1_teacher_referred_donor",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
)

fully_funded.eval <- make.gbm.train.model.estimate(
  variable="fully_funded",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
)

at_least_1_green_donation.eval <- make.gbm.train.model.estimate(
  variable="at_least_1_green_donation",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
)

great_chat.eval <- make.gbm.train.model.estimate(
  variable="great_chat",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
)

three_or_more_non_teacher_referred_donors.eval <- make.gbm.train.model.estimate(
  variable="three_or_more_non_teacher_referred_donors",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
)

is.exciting.eval <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
)

cat("auc at_least_1_teacher_referred_donor :",make.auc(at_least_1_teacher_referred_donor.eval), "\n")
cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
cat("auc at_least_1_green_donation :",make.auc(at_least_1_green_donation.eval), "\n")
cat("auc great_chat :",make.auc(great_chat.eval), "\n")
cat("auc three_or_more_non_teacher_referred_donors :",make.auc(three_or_more_non_teacher_referred_donors.eval), "\n")
cat("auc is_exciting :",make.auc(is.exciting.eval), "\n")


plot.response <- function(model.object, binwidth) {
  library(ggplot2)
  
  y <- model.object$data.test[,model.object$y.variable]
  
  model.cols <- rownames(model.object$importance)
  predicted <- predict(model.object$model, newdata=model.object$data.test[,model.cols],n.trees=model.object$n.trees, type="response")
  
  df <- data.frame(
    variable=model.object$y.variable,
    prediction=predicted,
    true.value=y
    )
  
  g <- ggplot(df) + geom_histogram(aes(x=prediction), binwidth=binwidth) + facet_grid(true.value ~ .) + xlim(0,1)
  
  return(g)
}
