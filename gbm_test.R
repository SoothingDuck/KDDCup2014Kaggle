source("functions.R")

projects.train <- get.projects.data.train(force=TRUE)

cat.cols <- get.categorical.vars()
num.cols <- get.numeric.vars()

all.cols <- union(cat.cols, num.cols)

# is.exciting
projects.train.is.exciting <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting <- subset(projects.train.is.exciting, days_since_posted <= 1350)

model.is.exciting.cols <- get.gbm.model.cols(
  xcols=all.cols,
  ycol="is_exciting",
  shrinkage = 1.0
  )

model.is.exciting <- get.gbm.model(
  xtrain=projects.train.is.exciting[,all.cols], 
  ytrain=projects.train.is.exciting[,c("is_exciting")], 
  shrinkage = 1.0)
