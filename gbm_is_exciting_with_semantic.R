source("functions.R")
source("variables.R")
load(file=file.path("tmp","donations_by_person_agg.RData"))

shrinkage.eval <- 0.01
n.trees.eval <- 450

is.exciting.eval <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval,
  percent.train=.7
)

# cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
library(ggplot2)
auc.list <- make.auc(is.exciting.eval, step.trees=5)
ggplot(auc.list) + geom_point(aes(x=n.tree, y=auc))


# estimate errors

prediction.eval <- predict(
  is.exciting.eval$model, 
  newdata=is.exciting.eval$data.test[,is.exciting.eval$important.cols],
  n.trees=n.trees.eval, 
  type="response"
)

data.test <- is.exciting.eval$data.test
data.test$prediction <- prediction.eval

library(ggplot2)
ggplot(data.test) + geom_boxplot(aes(x=is_exciting, y=prediction))


# fully_funded.eval <- make.gbm.train.model.estimate(
#   variable="fully_funded",
#   days.hist=nb.days,
#   shrinkage=shrinkage.eval,
#   n.trees=n.trees.eval
# )

# is.exciting
shrinkage.refined <- 0.01
n.trees.refined <- 1000

is.exciting.eval.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.refined,
  n.trees=n.trees.refined,
  model.cols=is.exciting.eval$important.cols,
  percent.train=.8
)

library(ggplot2)
auc.list <- make.auc(is.exciting.eval.refined, step.trees=5)
ggplot(auc.list) + geom_point(aes(x=n.tree, y=auc))

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

prediction.without.donators <- predict(
  is.exciting.eval.without.donators.refined$model, 
  newdata=test.data.without.donators[,is.exciting.eval.without.donators.refined$important.cols],
  n.trees=n.trees.refined, 
  type="response"
)

df.without.donators <- data.frame(
  projectid=test.data.without.donators$projectid,
  is_exciting=prediction.without.donators,
  stringsAsFactors=FALSE
)

df <- rbind(df.with.donators, df.without.donators)

write.csv(
  x=df, 
  file=file.path("submissions","gbm_submission_is_exciting_only_model.csv"),
  row.names=FALSE,
  quote=FALSE
  )
