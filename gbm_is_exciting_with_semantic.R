source("functions.R")
source("extract_donations.R")

make.auc <- function(model.object) {
  
  y <- model.object$data.test[,model.object$y.variable]
  
  model.cols <- rownames(model.object$importance)
  
  predicted <- predict(model.object$model, newdata=model.object$data.test[,model.cols],n.trees=model.object$n.trees, type="response")
  auc.value <- auc(y, predicted)
  
  return(auc.value)
}

make.model.variable.list <- function(data) {
  
  all.cols <- get.all.variables(data)
  all.cols <- union(all.cols, colnames(data)[grepl("word", colnames(data))])
  all.cols <- union(all.cols, c("total_donation_to_project", "total_donation_optional_support"))
  all.cols <- union(all.cols, c("mean_donation_to_project", "mean_donation_optional_support"))
  all.cols <- union(all.cols, c("total_donation_total", "mean_donation_total"))
  all.cols <- union(all.cols, c("min_days_since_donation", "max_days_since_donation","mean_days_since_donation"))
  
  model.cols <- all.cols
  # model.cols <- model.cols[! grepl("school_state", model.cols)]
  
  return(model.cols)
}

make.projects.train <-  function(variable, days.hist, force=FALSE, percent.train=0.7) {
  projects.train.is.exciting.all <- get.projects.data.train(force=force, variable=variable)
  projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= days.hist)
  
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
  
  projects.train.is.exciting.all <- merge(projects.train.is.exciting.all, donations.by.person.agg, by.x="teacher_acctid", by.y="donor_acctid", all.x=TRUE)
  projects.train.is.exciting.all$total_donation_to_project <- with(projects.train.is.exciting.all, ifelse(is.na(total_donation_to_project), 0, total_donation_to_project))
  projects.train.is.exciting.all$total_donation_optional_support <- with(projects.train.is.exciting.all, ifelse(is.na(total_donation_optional_support), 0, total_donation_optional_support))
  projects.train.is.exciting.all$mean_donation_to_project <- with(projects.train.is.exciting.all, ifelse(is.na(mean_donation_to_project), 0, mean_donation_to_project))
  projects.train.is.exciting.all$mean_donation_optional_support <- with(projects.train.is.exciting.all, ifelse(is.na(mean_donation_optional_support), 0, mean_donation_optional_support))
  projects.train.is.exciting.all$total_donation_total <- with(projects.train.is.exciting.all, ifelse(is.na(total_donation_total), 0, total_donation_total))
  projects.train.is.exciting.all$mean_donation_total <- with(projects.train.is.exciting.all, ifelse(is.na(mean_donation_total), 0, mean_donation_total))
  projects.train.is.exciting.all$min_days_since_donation <- with(projects.train.is.exciting.all, ifelse(is.na(min_days_since_donation), 5000, min_days_since_donation))
  projects.train.is.exciting.all$max_days_since_donation <- with(projects.train.is.exciting.all, ifelse(is.na(max_days_since_donation), 5000, max_days_since_donation))
  projects.train.is.exciting.all$mean_days_since_donation <- with(projects.train.is.exciting.all, ifelse(is.na(mean_days_since_donation), 5000, mean_days_since_donation))
  
  projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all, percent.train=percent.train)
  
  return(projects.train.is.exciting)
}

make.gbm.train.model.estimate <- function(variable, days.hist, shrinkage, n.trees, force=FALSE, percent.train=0.7) {
    
  projects.train.is.exciting <- make.projects.train(variable, days.hist, force=force, percent.train=percent.train)
  model.cols <- make.model.variable.list(projects.train.is.exciting$train)
      
  model.is.exciting <- get.gbm.model(
    xtrain=projects.train.is.exciting$train[,model.cols], 
    ytrain=projects.train.is.exciting$train[,c(variable)], 
    shrinkage = shrinkage,
    n.trees = n.trees
  )
  
  df <- data.frame(summary(model.is.exciting))
  important.cols <- as.character(df$var[df$rel.inf > 0.0])
  
  return(list(
    model=model.is.exciting,
    importance=df,
    important.cols=important.cols,
    data.train=projects.train.is.exciting$train,
    data.test=projects.train.is.exciting$test,
    y.variable=variable,
    n.trees=n.trees
    ))
}

make.gbm.train.model.important <- function(variable, days.hist, shrinkage, n.trees, model.cols, force=FALSE, percent.train=0.95) {
  
  projects.train.is.exciting <- make.projects.train(variable, days.hist, force=force, percent.train=percent.train)
  
  model.is.exciting <- get.gbm.model(
    xtrain=projects.train.is.exciting$train[,model.cols], 
    ytrain=projects.train.is.exciting$train[,c(variable)], 
    shrinkage = shrinkage,
    n.trees = n.trees
  )

  df <- data.frame(summary(model.is.exciting))
  
  return(list(
    model=model.is.exciting,
    importance=df,
    important.cols=model.cols,
    data.train=projects.train.is.exciting$train,
    data.test=projects.train.is.exciting$test,
    y.variable=variable,
    n.trees=n.trees
  ))
}

is.exciting.eval <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=350,
  shrinkage=0.1,
  n.trees=200
  )

fully_funded.eval <- make.gbm.train.model.estimate(
  variable="fully_funded",
  days.hist=350,
  shrinkage=0.1,
  n.trees=200
)

cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
cat("auc is_exciting :",make.auc(is.exciting.eval), "\n")

stop()
# is.exciting

is.exciting.eval.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=350,
  shrinkage=0.01,
  n.trees=1000,
  model.cols=is.exciting.eval$important.cols
)
cat("auc is_exciting :",make.auc(is.exciting.eval.refined), "\n")

# model on everything
model.is.exciting.only.refined <- get.gbm.model(
  xtrain=projects.train.is.exciting.all[,important.cols], 
  ytrain=projects.train.is.exciting.all[,c("is_exciting")], 
  shrinkage = 0.01,
  n.trees = 1000)

# valider une soumission
test.data <- get.projects.data.test(force=FALSE)

test.data <- merge(test.data, semantic.item_name.data, by="projectid")
test.data <- merge(test.data, semantic.short_description.data, by="projectid")
test.data <- merge(test.data, semantic.title.data, by="projectid")
test.data <- merge(test.data, semantic.essay.data, by="projectid")
test.data <- merge(test.data, semantic.need_statement.data, by="projectid")

test.data <- merge(test.data, donations.by.person.agg, by.x="teacher_acctid", by.y="donor_acctid", all.x=TRUE)
test.data$total_donation_to_project <- with(test.data, ifelse(is.na(total_donation_to_project), 0, total_donation_to_project))
test.data$total_donation_optional_support <- with(test.data, ifelse(is.na(total_donation_optional_support), 0, total_donation_optional_support))

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
