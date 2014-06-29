source("functions.R")
source("variables.R")
load(file=file.path("tmp","donations_by_person_agg.RData"))

shrinkage.eval <- 0.1
n.trees.eval <- 300

is.exciting.eval <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.eval,
  n.trees=n.trees.eval
  )

# fully_funded.eval <- make.gbm.train.model.estimate(
#   variable="fully_funded",
#   days.hist=nb.days,
#   shrinkage=shrinkage.eval,
#   n.trees=n.trees.eval
# )

# cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
cat("auc is_exciting :",make.auc(is.exciting.eval), "\n")

# is.exciting
shrinkage.refined <- 0.05
n.trees.refined <- 1000

is.exciting.eval.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=shrinkage.refined,
  n.trees=n.trees.refined,
  model.cols=is.exciting.eval$important.cols
)

cat("auc is_exciting :",make.auc(is.exciting.eval.refined), "\n")


test.data <- make.projects.test(force=FALSE)

prediction <- predict(
  is.exciting.eval.refined$model, 
  newdata=test.data[,is.exciting.eval$important.cols],
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
