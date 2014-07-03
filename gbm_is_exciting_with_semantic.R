source("functions.R")
source("variables.R")
load(file=file.path("tmp","donations_by_person_agg.RData"))

shrinkage.eval <- 0.1
n.trees.eval <- 200

# data <- make.projects.train("is_exciting", days.hist=400, force=FALSE, percent.train=0.95)
# data.train <- data$train
# data.test <- make.projects.test(force=FALSE)
# 
# library(ggplot2)
# ggplot() + geom_histogram(aes(x=days_since_posted,fill="train"), binwidth=5, data=data.train) +
#   geom_histogram(aes(x=days_since_posted,fill="test"), binwidth=5, data=data.test)

is.exciting.eval <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval,
  percent.train=.7,
  interaction.depth = 2
)

# cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
# library(ggplot2)
# auc.list <- make.auc(is.exciting.eval, step.trees=5)
# ggplot(auc.list) + geom_point(aes(x=n.tree, y=auc))


# estimate errors

# prediction.eval <- predict(
#   is.exciting.eval$model, 
#   newdata=is.exciting.eval$data.test[,is.exciting.eval$important.cols],
#   n.trees=n.trees.eval, 
#   type="response"
# )
# 
# data.test <- is.exciting.eval$data.test
# data.test$prediction <- prediction.eval

# library(ggplot2)
# ggplot(data.test) + geom_boxplot(aes(x=is_exciting, y=prediction))


# fully_funded.eval <- make.gbm.train.model.estimate(
#   variable="fully_funded",
#   days.hist=nb.days,
#   shrinkage=shrinkage.eval,
#   n.trees=n.trees.eval
# )

# is.exciting
shrinkage.refined <- 0.01
n.trees.refined <- 2000

is.exciting.eval.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.refined,
  n.trees=n.trees.refined,
  model.cols=is.exciting.eval$important.cols,
  percent.train=.9,
  interaction.depth = 2
)

# library(ggplot2)
# auc.list <- make.auc(is.exciting.eval.refined, step.trees=10)
# ggplot(auc.list) + geom_point(aes(x=n.tree, y=auc))

# estimate error refined
# prediction.eval.refined <- predict(
#   is.exciting.eval.refined$model, 
#   newdata=is.exciting.eval.refined$data.test[,is.exciting.eval.refined$important.cols],
#   n.trees=n.trees.refined, 
#   type="response"
# )
# 
# data.test <- is.exciting.eval.refined$data.test
# data.test$prediction <- prediction.eval.refined
# 
# library(ggplot2)
# ggplot(data.test) + geom_boxplot(aes(x=is_exciting, y=prediction))


test.data <- make.projects.test(force=FALSE)

prediction <- predict(
  is.exciting.eval.refined$model, 
  newdata=test.data[,is.exciting.eval.refined$important.cols],
  n.trees=n.trees.refined, 
  type="response"
  )

df <- data.frame(
  projectid=test.data$projectid,
  is_exciting=prediction,
  stringsAsFactors=FALSE
  )

write.csv(
  x=df, 
  file=file.path("submissions","gbm_submission_is_exciting_only_model.csv"),
  row.names=FALSE,
  quote=FALSE
  )
