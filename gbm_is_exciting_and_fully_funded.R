source("functions.R")

# fully_funded
model.filename <- file.path("tmp","model_fully_funded.RData")

projects.train.fully_funded.all <- get.projects.data.train(force=FALSE, variable="fully_funded")
projects.train.fully_funded.all <- subset(projects.train.fully_funded.all, days_since_posted <= 350)

all.cols <- get.all.variables(projects.train.fully_funded.all)
model.cols <- all.cols

projects.train.fully_funded <- split.train.test(projects.train.fully_funded.all)

model.fully_funded <- get.gbm.model(
  xtrain=projects.train.fully_funded$train[,all.cols], 
  ytrain=projects.train.fully_funded$train[,c("fully_funded")], 
  shrinkage = 0.1,
  n.trees = 200
)

df <- data.frame(summary(model.fully_funded), stringsAsFactor=FALSE)
important.cols.fully_funded <- as.character(df$var[df$rel.inf > 0.0])

# model on everything
model.fully_funded.only.refined <- get.gbm.model(
  xtrain=projects.train.fully_funded.all[,important.cols.fully_funded], 
  ytrain=projects.train.fully_funded.all[,c("fully_funded")], 
  shrinkage = 0.1,
  n.trees = 500)

# is.exciting
model.filename <- file.path("tmp","model_is_exciting.RData")

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 350)

all.cols <- get.all.variables(projects.train.is.exciting.all)
model.cols <- all.cols

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.is.exciting <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,all.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 0.1,
  n.trees = 200
)

df <- data.frame(summary(model.is.exciting), stringsAsFactor=FALSE)
important.cols.is.exciting <- as.character(df$var[df$rel.inf > 0.0])

# model on everything
model.is.exciting.only.refined <- get.gbm.model(
  xtrain=projects.train.is.exciting.all[,important.cols.is.exciting], 
  ytrain=projects.train.is.exciting.all[,c("is_exciting")], 
  shrinkage = 0.1,
  n.trees = 500)

# valider une soumission
test.data <- get.projects.data.test(force=FALSE)

prediction.is_exciting <- predict(model.is.exciting.only.refined, newdata=test.data[,important.cols.is.exciting],n.trees=500, type="response")
prediction.fully_funded <- predict(model.fully_funded.only.refined, newdata=test.data[,important.cols.fully_funded],n.trees=500, type="response")

prediction <- ifelse(prediction.fully_funded < 0.5, prediction.is_exciting*(prediction.fully_funded/0.5), prediction.is_exciting)

df <- data.frame(
  projectid=test.data$projectid,
  is_exciting=prediction,
  stringsAsFactors=FALSE
)

write.csv(
  x=df, 
  file=file.path("submissions","gbm_submission_is_exciting_and_fully_funded.csv"),
  row.names=FALSE,
  quote=FALSE
)
