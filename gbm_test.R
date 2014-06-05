source("functions.R")
library(randomForest)
library(gbm)

projects.data <- get.projects.data(force=FALSE, with.outcomes=TRUE)
projects.data.train <- projects.data

projects.data <- get.projects.data(force=FALSE, with.outcomes=FALSE)
projects.data.test <- subset(projects.data, typedataset == "test")

model.list.projects.outcomes.filename <- file.path("tmp","model_random_forest_projects_outcomes.RData")
load(model.list.projects.outcomes.filename)

model.is_exciting <- model.list.projects.outcomes[["is_exciting"]]

col.model <- c(rownames(model.is_exciting$importance), "secondary_focus_area", "secondary_focus_subject", "school_state")

model <- gbm.fit(
  x=projects.data[,col.model],
  y=ifelse(projects.data$is_exciting == "Yes", 1, 0),
  shrinkage=1,
  verbose=TRUE
  )

pred <- predict(model, newdata=projects.data.test[,col.model], n.trees=100, type="response")

df <- data.frame(
  projectid=projects.data.test$projectid,
  is_exciting=pred,
  stringsAsFactors=FALSE
)


submission.filename <- file.path("submissions","submission_gbm_test.csv")

write.csv(df, file=submission.filename, row.names=FALSE, quote=FALSE)
