source("functions.R")

# is.exciting
model.filename <- file.path("tmp","model_is_exciting.RData")

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE, variable="is_exciting")
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 350)

load(file=file.path("tmp","semantic_item_name.RData"))
load(file=file.path("tmp","semantic_short_description.RData"))
load(file=file.path("tmp","semantic_title.RData"))
load(file=file.path("tmp","semantic_essay.RData"))
load(file=file.path("tmp","semantic_need_statement.RData"))

projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, semantic.item_name.data, by="projectid")
projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, semantic.short_description.data, by="projectid")
projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, semantic.title.data, by="projectid")
projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, semantic.essay.data, by="projectid")
projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, semantic.need_statement.data, by="projectid")

all.cols <- get.all.variables(projects.train.is.exciting.all)
all.cols <- union(all.cols, colnames(projects.train.is.exciting.all)[grepl("word", colnames(projects.train.is.exciting.all))])

model.cols <- all.cols
# model.cols <- model.cols[! grepl("school_state", model.cols)]

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
  shrinkage = 0.05,
  n.trees = 1000)

# valider une soumission
test.data <- get.projects.data.test(force=FALSE)

test.data <- merge(test.data, semantic.item_name.data, by="projectid")
test.data <- merge(test.data, semantic.short_description.data, by="projectid")
test.data <- merge(test.data, semantic.title.data, by="projectid")
test.data <- merge(test.data, semantic.essay.data, by="projectid")
test.data <- merge(test.data, semantic.need_statement.data, by="projectid")

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
