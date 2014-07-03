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
shrinkage.refined <- 0.02
n.trees.refined <- 1000

is.exciting.eval.with.donators.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.refined,
  n.trees=n.trees.refined,
  model.cols=is.exciting.eval.with.donators$important.cols,
  with.donators=TRUE
)

is.exciting.eval.without.donators.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.refined,
  n.trees=n.trees.refined,
  model.cols=is.exciting.eval.without.donators$important.cols,
  with.donators=FALSE
)

cat("auc is_exciting with    donators :",make.auc(is.exciting.eval.with.donators.refined), "\n")
cat("auc is_exciting without donators :",make.auc(is.exciting.eval.without.donators.refined), "\n")

test.data.with.donators <- make.projects.test(force=FALSE, with.donators=TRUE)
test.data.without.donators <- make.projects.test(force=FALSE, with.donators=FALSE)

prediction.with.donators <- predict(
  is.exciting.eval.with.donators.refined$model, 
  newdata=test.data.with.donators[,is.exciting.eval.with.donators.refined$important.cols],
  n.trees=n.trees.refined, 
  type="response"
  )

df.with.donators <- data.frame(
  projectid=test.data.with.donators$projectid,
  is_exciting=prediction.with.donators,
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
