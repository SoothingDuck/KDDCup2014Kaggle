source("functions.R")

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


# fully_funded
projects.train.fully_funded.all <- get.projects.data.train(force=FALSE, variable="fully_funded")

projects.train.fully_funded <- split.train.test(projects.train.fully_funded.all)

model.fully_funded <- get.gbm.model(
  xtrain=projects.train.fully_funded$train[,all.cols], 
  ytrain=projects.train.fully_funded$train[,c("fully_funded")], 
  shrinkage = 1.0)

prediction.fully_funded <- predict(model.fully_funded, newdata=projects.train.fully_funded$test[,all.cols], n.trees=100, type="response")
cat("auc fully_funded :", auc(y=projects.train.fully_funded$test[,c("fully_funded")],predicted=prediction.fully_funded), "\n")


# at_least_1_green_donation
projects.train.at_least_1_green_donation.all <- get.projects.data.train(force=FALSE, variable="at_least_1_green_donation")

projects.train.at_least_1_green_donation <- split.train.test(projects.train.at_least_1_green_donation.all)

model.at_least_1_green_donation <- get.gbm.model(
  xtrain=projects.train.at_least_1_green_donation$train[,all.cols], 
  ytrain=projects.train.at_least_1_green_donation$train[,c("at_least_1_green_donation")], 
  shrinkage = 1.0)

prediction.at_least_1_green_donation <- predict(model.at_least_1_green_donation, newdata=projects.train.at_least_1_green_donation$test[,all.cols], n.trees=100, type="response")
cat("auc at_least_1_green_donation :", auc(y=projects.train.at_least_1_green_donation$test[,c("at_least_1_green_donation")],predicted=prediction.at_least_1_green_donation), "\n")
