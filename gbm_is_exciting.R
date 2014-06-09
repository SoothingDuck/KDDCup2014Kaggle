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

# with essays
source("extract_essays.R")

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 1350)

projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, essays.data, by=c("projectid"))

essays.col <- names(essays.data)[names(essays.data) != "projectid"]

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.cols <- union(important.cols, essays.col)

model.is.exciting.essays <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,model.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 1.0)

# model on everything
model.is.exciting.essays <- get.gbm.model(
  xtrain=projects.train.is.exciting.all[,model.cols], 
  ytrain=projects.train.is.exciting.all[,c("is_exciting")], 
  shrinkage = 0.5,
  n.trees = 200)

# valider une soumission

