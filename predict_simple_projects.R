source("functions.R")
library(randomForest)

projects.data <- get.projects.data(force=FALSE, with.outcomes=FALSE)

model.list.projects.outcomes.filename <- file.path("tmp","model_random_forest_projects_outcomes.RData")
load(model.list.projects.outcomes.filename)

projects.data.test <- subset(projects.data, typedataset == "test")

model.is_exciting <- model.list.projects.outcomes[["is_exciting"]]

col.model <- rownames(model.is_exciting$importance)

pred <- predict(model.is_exciting, newdata=projects.data.test[,col.model])
pred.prob <- predict(model.is_exciting, newdata=projects.data.test[,col.model], type="prob")
pred.prob.Yes <- pred.prob[,2]

df <- data.frame(
  projectid=projects.data.test$projectid,
  is_exciting=pred.prob.Yes,
  stringsAsFactors=FALSE
    )


submission.filename <- file.path("submissions","submission_predict_simple_projects.csv")

write.csv(df, file=submission.filename, row.names=FALSE, quote=FALSE)
