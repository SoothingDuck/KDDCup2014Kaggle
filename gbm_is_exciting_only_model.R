source("functions.R")

all.cols <- get.all.variables()

# is.exciting
model.filename <- file.path("tmp","model_is_exciting.RData")

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 1350)

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.is.exciting <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,all.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 1.0)

df <- data.frame(summary(model.is.exciting), stringsAsFactor=FALSE)
important.cols <- as.character(df$var[df$rel.inf > 0.0])

# model on everything
model.is.exciting.essays <- get.gbm.model(
  xtrain=projects.train.is.exciting.all[,model.cols], 
  ytrain=projects.train.is.exciting.all[,c("is_exciting")], 
  shrinkage = 0.5,
  n.trees = 200)

# valider une soumission
test.data <- get.projects.data.test(force=TRUE)
test.data <- merge(test.data, essays.data, by=c("projectid"))

prediction <- predict(model.is.exciting.essays, newdata=test.data[,model.cols],n.trees=200, type="response")

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
