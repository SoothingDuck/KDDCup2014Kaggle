source("functions.R")
source("variables.R")
load(file=file.path("tmp","donations_by_person_agg.RData"))

shrinkage.eval <- 0.2
n.trees.eval <- 50

is.exciting.eval.with.donators <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval,
  with.donators=TRUE,
  percent.train=.95
)

is.exciting.eval.without.donators <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval,
  with.donators=FALSE,
  percent.train=.95
  )

# fully_funded.eval <- make.gbm.train.model.estimate(
#   variable="fully_funded",
#   days.hist=nb.days,
#   shrinkage=shrinkage.eval,
#   n.trees=n.trees.eval
# )

# cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
cat("auc is_exciting with donators    :",make.auc(is.exciting.eval.with.donators), "\n")
cat("auc is_exciting without donators :",make.auc(is.exciting.eval.without.donators), "\n")

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
