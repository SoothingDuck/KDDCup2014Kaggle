source("functions.R")

cat.cols <- get.categorical.vars()
num.cols <- get.numeric.vars()
all.cols <- union(cat.cols, num.cols)

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

# with essays and resources
source("extract_essays.R")
source("extract_resources.R")

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 1350)

projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, essays.data, by=c("projectid"))
projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, resources.by.type, by=c("projectid"))

essays.col <- names(essays.data)[names(essays.data) != "projectid"]
resources.col <- names(resources.by.type)[names(resources.by.type) != "projectid"]

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.cols <- union(important.cols, essays.col)
model.cols <- union(model.cols, resources.col)

model.is.exciting.essays.resources <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,model.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 1.0)

# model on everything
model.is.exciting.essays.resources <- get.gbm.model(
  xtrain=projects.train.is.exciting.all[,model.cols], 
  ytrain=projects.train.is.exciting.all[,c("is_exciting")], 
  shrinkage = 0.5,
  n.trees = 200)

# valider une soumission
test.data <- get.projects.data.test(force=FALSE)
test.data <- merge(test.data, essays.data, by=c("projectid"))
test.data <- merge(test.data, resources.by.type, by=c("projectid"), all.x=TRUE, incomparables=0)
test.data[is.na(test.data)] <- 0

prediction <- predict(model.is.exciting.essays.resources, newdata=test.data[,model.cols],n.trees=200, type="response")

df <- data.frame(
  projectid=test.data$projectid,
  is_exciting=prediction,
  stringsAsFactors=FALSE
  )

write.csv(
  x=df, 
  file=file.path("tmp","gbm_submission_with_essays_and_resources.csv"),
  row.names=FALSE,
  quote=FALSE
  )
