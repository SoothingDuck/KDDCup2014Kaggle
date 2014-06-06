source("functions.R")

cat.cols <- get.categorical.vars()
num.cols <- get.numeric.vars()

all.cols <- union(cat.cols, num.cols)

result.auc <- data.frame()

# is.exciting
projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 1350)

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.is.exciting <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,all.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 1.0)

prediction.is.exciting <- predict(model.is.exciting, newdata=projects.train.is.exciting$test[,all.cols], n.trees=100, type="response")
cat("auc is_exciting :", auc(y=projects.train.is.exciting$test[,c("is_exciting")],predicted=prediction.is.exciting), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="is_exciting",
                      auc=auc(y=projects.train.is.exciting$test[,c("is_exciting")],predicted=prediction.is.exciting)
                      ))

# at_least_1_teacher_referred_donor
projects.train.at_least_1_teacher_referred_donor.all <- get.projects.data.train(force=FALSE, variable="at_least_1_teacher_referred_donor")
projects.train.at_least_1_teacher_referred_donor.all <- subset(projects.train.at_least_1_teacher_referred_donor.all, days_since_posted <= 1350)

projects.train.at_least_1_teacher_referred_donor <- split.train.test(projects.train.at_least_1_teacher_referred_donor.all)

model.at_least_1_teacher_referred_donor <- get.gbm.model(
  xtrain=projects.train.at_least_1_teacher_referred_donor$train[,all.cols], 
  ytrain=projects.train.at_least_1_teacher_referred_donor$train[,c("at_least_1_teacher_referred_donor")], 
  shrinkage = 1.0)

prediction.at_least_1_teacher_referred_donor <- predict(model.at_least_1_teacher_referred_donor, newdata=projects.train.at_least_1_teacher_referred_donor$test[,all.cols], n.trees=100, type="response")
cat("auc at_least_1_teacher_referred_donor :", auc(y=projects.train.at_least_1_teacher_referred_donor$test[,c("at_least_1_teacher_referred_donor")],predicted=prediction.at_least_1_teacher_referred_donor), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="at_least_1_teacher_referred_donor",
                      auc=auc(y=projects.train.at_least_1_teacher_referred_donor$test[,c("at_least_1_teacher_referred_donor")],predicted=prediction.at_least_1_teacher_referred_donor)
                    ))


# fully_funded
projects.train.fully_funded.all <- get.projects.data.train(force=FALSE, variable="fully_funded")

projects.train.fully_funded <- split.train.test(projects.train.fully_funded.all)

model.fully_funded <- get.gbm.model(
  xtrain=projects.train.fully_funded$train[,all.cols], 
  ytrain=projects.train.fully_funded$train[,c("fully_funded")], 
  shrinkage = 1.0)

prediction.fully_funded <- predict(model.fully_funded, newdata=projects.train.fully_funded$test[,all.cols], n.trees=100, type="response")
cat("auc fully_funded :", auc(y=projects.train.fully_funded$test[,c("fully_funded")],predicted=prediction.fully_funded), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="fully_funded",
                      auc=auc(y=projects.train.fully_funded$test[,c("fully_funded")],predicted=prediction.fully_funded)
                    ))


# at_least_1_green_donation
projects.train.at_least_1_green_donation.all <- get.projects.data.train(force=FALSE, variable="at_least_1_green_donation")

projects.train.at_least_1_green_donation <- split.train.test(projects.train.at_least_1_green_donation.all)

model.at_least_1_green_donation <- get.gbm.model(
  xtrain=projects.train.at_least_1_green_donation$train[,all.cols], 
  ytrain=projects.train.at_least_1_green_donation$train[,c("at_least_1_green_donation")], 
  shrinkage = 1.0)

prediction.at_least_1_green_donation <- predict(model.at_least_1_green_donation, newdata=projects.train.at_least_1_green_donation$test[,all.cols], n.trees=100, type="response")
cat("auc at_least_1_green_donation :", auc(y=projects.train.at_least_1_green_donation$test[,c("at_least_1_green_donation")],predicted=prediction.at_least_1_green_donation), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="at_least_1_green_donation",
                      auc=auc(y=projects.train.at_least_1_green_donation$test[,c("at_least_1_green_donation")],predicted=prediction.at_least_1_green_donation)
                    ))

# great_chat
projects.train.great_chat.all <- get.projects.data.train(force=FALSE, variable="great_chat")
projects.train.great_chat.all <- subset(projects.train.great_chat.all, days_since_posted <= 2100)

projects.train.great_chat <- split.train.test(projects.train.great_chat.all)

model.great_chat <- get.gbm.model(
  xtrain=projects.train.great_chat$train[,all.cols], 
  ytrain=projects.train.great_chat$train[,c("great_chat")], 
  shrinkage = 1.0)

prediction.great_chat <- predict(model.great_chat, newdata=projects.train.great_chat$test[,all.cols], n.trees=100, type="response")
cat("auc great_chat :", auc(y=projects.train.great_chat$test[,c("great_chat")],predicted=prediction.great_chat), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="great_chat",
                      auc=auc(y=projects.train.great_chat$test[,c("great_chat")],predicted=prediction.great_chat)
                    ))

# three_or_more_non_teacher_referred_donors
projects.train.three_or_more_non_teacher_referred_donors.all <- get.projects.data.train(force=FALSE, variable="three_or_more_non_teacher_referred_donors")
#projects.train.three_or_more_non_teacher_referred_donors.all <- subset(projects.train.three_or_more_non_teacher_referred_donors.all, days_since_posted <= 2100)

projects.train.three_or_more_non_teacher_referred_donors <- split.train.test(projects.train.three_or_more_non_teacher_referred_donors.all)

model.three_or_more_non_teacher_referred_donors <- get.gbm.model(
  xtrain=projects.train.three_or_more_non_teacher_referred_donors$train[,all.cols], 
  ytrain=projects.train.three_or_more_non_teacher_referred_donors$train[,c("three_or_more_non_teacher_referred_donors")], 
  shrinkage = 1.0)

prediction.three_or_more_non_teacher_referred_donors <- predict(model.three_or_more_non_teacher_referred_donors, newdata=projects.train.three_or_more_non_teacher_referred_donors$test[,all.cols], n.trees=100, type="response")
cat("auc three_or_more_non_teacher_referred_donors :", auc(y=projects.train.three_or_more_non_teacher_referred_donors$test[,c("three_or_more_non_teacher_referred_donors")],predicted=prediction.three_or_more_non_teacher_referred_donors), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="three_or_more_non_teacher_referred_donors",
                      auc=auc(y=projects.train.three_or_more_non_teacher_referred_donors$test[,c("three_or_more_non_teacher_referred_donors")],predicted=prediction.three_or_more_non_teacher_referred_donors)
                    ))


# one_non_teacher_referred_donor_giving_100_plus
projects.train.one_non_teacher_referred_donor_giving_100_plus.all <- get.projects.data.train(force=FALSE, variable="one_non_teacher_referred_donor_giving_100_plus")
#projects.train.one_non_teacher_referred_donor_giving_100_plus.all <- subset(projects.train.one_non_teacher_referred_donor_giving_100_plus.all, days_since_posted <= 2100)

projects.train.one_non_teacher_referred_donor_giving_100_plus <- split.train.test(projects.train.one_non_teacher_referred_donor_giving_100_plus.all)

model.one_non_teacher_referred_donor_giving_100_plus <- get.gbm.model(
  xtrain=projects.train.one_non_teacher_referred_donor_giving_100_plus$train[,all.cols], 
  ytrain=projects.train.one_non_teacher_referred_donor_giving_100_plus$train[,c("one_non_teacher_referred_donor_giving_100_plus")], 
  shrinkage = 1.0)

prediction.one_non_teacher_referred_donor_giving_100_plus <- predict(model.one_non_teacher_referred_donor_giving_100_plus, newdata=projects.train.one_non_teacher_referred_donor_giving_100_plus$test[,all.cols], n.trees=100, type="response")
cat("auc one_non_teacher_referred_donor_giving_100_plus :", auc(y=projects.train.one_non_teacher_referred_donor_giving_100_plus$test[,c("one_non_teacher_referred_donor_giving_100_plus")],predicted=prediction.one_non_teacher_referred_donor_giving_100_plus), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="one_non_teacher_referred_donor_giving_100_plus",
                      auc=auc(y=projects.train.one_non_teacher_referred_donor_giving_100_plus$test[,c("one_non_teacher_referred_donor_giving_100_plus")],predicted=prediction.one_non_teacher_referred_donor_giving_100_plus)
                    ))


# donation_from_thoughtful_donor
projects.train.donation_from_thoughtful_donor.all <- get.projects.data.train(force=FALSE, variable="donation_from_thoughtful_donor")
projects.train.donation_from_thoughtful_donor.all <- subset(projects.train.donation_from_thoughtful_donor.all, days_since_posted <= 2000)

projects.train.donation_from_thoughtful_donor <- split.train.test(projects.train.donation_from_thoughtful_donor.all)

model.donation_from_thoughtful_donor <- get.gbm.model(
  xtrain=projects.train.donation_from_thoughtful_donor$train[,all.cols], 
  ytrain=projects.train.donation_from_thoughtful_donor$train[,c("donation_from_thoughtful_donor")], 
  shrinkage = 1.0)

prediction.donation_from_thoughtful_donor <- predict(model.donation_from_thoughtful_donor, newdata=projects.train.donation_from_thoughtful_donor$test[,all.cols], n.trees=100, type="response")
cat("auc donation_from_thoughtful_donor :", auc(y=projects.train.donation_from_thoughtful_donor$test[,c("donation_from_thoughtful_donor")],predicted=prediction.donation_from_thoughtful_donor), "\n")
result.auc <- rbind(result.auc,
                    data.frame(
                      variable.name="donation_from_thoughtful_donor",
                      auc=auc(y=projects.train.donation_from_thoughtful_donor$test[,c("donation_from_thoughtful_donor")],predicted=prediction.donation_from_thoughtful_donor)
                    ))

write.csv(x=result.auc, file=file.path("tmp","result_auc.csv"))
