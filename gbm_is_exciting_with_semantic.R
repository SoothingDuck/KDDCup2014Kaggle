source("functions.R")

# is.exciting
model.filename <- file.path("tmp","model_is_exciting.RData")

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 350)

load(file=file.path("tmp","semantic.RData"))
projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, semantic.data, by="projectid")

all.cols <- get.all.variables(projects.train.is.exciting.all)
all.cols <- union(all.cols, colnames(projects.train.is.exciting.all)[grepl("word", colnames(projects.train.is.exciting.all))])

model.cols <- all.cols

projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all)

model.is.exciting <- get.gbm.model(
  xtrain=projects.train.is.exciting$train[,all.cols], 
  ytrain=projects.train.is.exciting$train[,c("is_exciting")], 
  shrinkage = 0.1,
  n.trees = 200
  )

df <- data.frame(summary(model.is.exciting), stringsAsFactor=FALSE)
important.cols <- as.character(df$var[df$rel.inf > 0.0])

# model on everything
model.is.exciting.only.refined <- get.gbm.model(
  xtrain=projects.train.is.exciting.all[,important.cols], 
  ytrain=projects.train.is.exciting.all[,c("is_exciting")], 
  shrinkage = 0.1,
  n.trees = 500)

# valider une soumission
test.data <- get.projects.data.test(force=FALSE)

prediction <- predict(model.is.exciting.only.refined, newdata=test.data[,important.cols],n.trees=500, type="response")

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
