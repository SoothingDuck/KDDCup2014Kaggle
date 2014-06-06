source("functions.R")

projects.train <- get.projects.data.train(force=TRUE)

cat.cols <- get.categorical.vars()
num.cols <- get.numeric.vars()

all.cols <- union(cat.cols, num.cols)

# is.exciting
projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 1350)

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.is.exciting.cols <- get.gbm.model.cols(
  xcols=all.cols,
  ycol="is_exciting",
  shrinkage = 1.0
  )

model.is.exciting <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,all.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 1.0)

prediction.is.exciting <- predict(model.is.exciting, newdata=projects.train.is.exciting$test[,all.cols], n.trees=100, type="response")
cat("auc is_exciting :", auc(y=projects.train.is.exciting$test[,c("is_exciting")],predicted=prediction.is.exciting), "\n")

# at_least_1_teacher_referred_donor
projects.train.at_least_1_teacher_referred_donor.all <- get.projects.data.train(force=FALSE, variable="at_least_1_teacher_referred_donor")
projects.train.at_least_1_teacher_referred_donor.all <- subset(projects.train.at_least_1_teacher_referred_donor.all, days_since_posted <= 1350)

projects.train.at_least_1_teacher_referred_donor <- split.train.test(projects.train.at_least_1_teacher_referred_donor.all)

model.at_least_1_teacher_referred_donor.cols <- get.gbm.model.cols(
  xcols=all.cols,
  ycol="at_least_1_teacher_referred_donor",
  shrinkage = 1.0
)

model.at_least_1_teacher_referred_donor <- get.gbm.model(
  xtrain=projects.train.at_least_1_teacher_referred_donor$train[,all.cols], 
  ytrain=projects.train.at_least_1_teacher_referred_donor$train[,c("at_least_1_teacher_referred_donor")], 
  shrinkage = 1.0)

prediction.at_least_1_teacher_referred_donor <- predict(model.at_least_1_teacher_referred_donor, newdata=projects.train.at_least_1_teacher_referred_donor$test[,all.cols], n.trees=100, type="response")
cat("auc at_least_1_teacher_referred_donor :", auc(y=projects.train.at_least_1_teacher_referred_donor$test[,c("at_least_1_teacher_referred_donor")],predicted=prediction.at_least_1_teacher_referred_donor), "\n")
