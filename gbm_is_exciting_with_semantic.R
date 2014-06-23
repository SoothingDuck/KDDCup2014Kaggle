source("functions.R")
source("extract_donations.R")

rm(list=c("donations.data"))
gc(TRUE)

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

#   total_donation_to_project=sum(donation_to_project),
#   max_donation_to_project=max(donation_to_project),
#   mean_donation_to_project=mean(donation_to_project),
#   median_donation_to_project=median(donation_to_project),
  all.cols <- union(
    all.cols,
    c(
      "total_donation_to_project", 
      "max_donation_to_project",
      "mean_donation_to_project",
      "median_donation_to_project"
      )
    )
  
#   total_donation_optional_support=sum(donation_optional_support),
#   max_donation_optional_support=max(donation_optional_support),
#   mean_donation_optional_support=mean(donation_optional_support),
#   median_donation_optional_support=median(donation_optional_support),
  all.cols <- union(
    all.cols, 
    c(
      "total_donation_optional_support", 
      "max_donation_optional_support",
      "mean_donation_optional_support",
      "median_donation_optional_support"
      )
    )

#   total_donation_total=sum(donation_to_project+donation_optional_support),
#   max_donation_total=max(donation_to_project+donation_optional_support),
#   mean_donation_total=mean(donation_to_project+donation_optional_support),
#   median_donation_total=median(donation_to_project+donation_optional_support),
  all.cols <- union(
    all.cols, 
    c(
      "total_donation_total", 
      "max_donation_total",
      "mean_donation_total",
      "median_donation_total"
      )
    )


#   min_days_since_donation=min(days_since_donation),
#   max_days_since_donation=max(days_since_donation),
#   mean_days_since_donation=mean(days_since_donation),
#   median_days_since_donation=median(days_since_donation)  
  all.cols <- union(
    all.cols, 
    c(
      "min_days_since_donation", 
      "max_days_since_donation",
      "mean_days_since_donation",
      "median_days_since_donation"
      ))
  
  model.cols <- all.cols
  # model.cols <- model.cols[! grepl("school_state", model.cols)]
  
  return(model.cols)
}

operations.on.data.set <- function(data) {
  load(file=file.path("tmp","semantic_item_name.RData"))
  load(file=file.path("tmp","semantic_short_description.RData"))
  load(file=file.path("tmp","semantic_title.RData"))
  load(file=file.path("tmp","semantic_essay.RData"))
  load(file=file.path("tmp","semantic_need_statement.RData"))
  
  data <- merge(data, semantic.item_name.data, by="projectid")
  data <- merge(data, semantic.short_description.data, by="projectid")
  data <- merge(data, semantic.title.data, by="projectid")
  data <- merge(data, semantic.essay.data, by="projectid")
  data <- merge(data, semantic.need_statement.data, by="projectid")
  
  data <- merge(data, donations.by.person.agg, by.x="teacher_acctid", by.y="donor_acctid", all.x=TRUE)
  data$total_donation_to_project <- with(data, ifelse(is.na(total_donation_to_project), 0, total_donation_to_project))
  data$total_donation_optional_support <- with(data, ifelse(is.na(total_donation_optional_support), 0, total_donation_optional_support))
  data$mean_donation_to_project <- with(data, ifelse(is.na(mean_donation_to_project), 0, mean_donation_to_project))
  data$mean_donation_optional_support <- with(data, ifelse(is.na(mean_donation_optional_support), 0, mean_donation_optional_support))
  data$total_donation_total <- with(data, ifelse(is.na(total_donation_total), 0, total_donation_total))
  data$mean_donation_total <- with(data, ifelse(is.na(mean_donation_total), 0, mean_donation_total))
  data$min_days_since_donation <- with(data, ifelse(is.na(min_days_since_donation), 5000, min_days_since_donation))
  data$max_days_since_donation <- with(data, ifelse(is.na(max_days_since_donation), 5000, max_days_since_donation))
  data$mean_days_since_donation <- with(data, ifelse(is.na(mean_days_since_donation), 5000, mean_days_since_donation))
  
  return(data)
}

make.projects.train <-  function(variable, days.hist, force=FALSE, percent.train=0.7) {
  projects.train.is.exciting.all <- get.projects.data.train(force=force, variable=variable)
  projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= days.hist)
  
  projects.train.is.exciting.all <- operations.on.data.set(projects.train.is.exciting.all)
  
  projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all, percent.train=percent.train)
  
  return(projects.train.is.exciting)
}

make.projects.test <-  function(force=FALSE) {
  projects.train.is.exciting.all <- get.projects.data.test(force=force)
  
  projects.train.is.exciting.all <- operations.on.data.set(projects.train.is.exciting.all)
  
  # projects.train.is.exciting <- split.train.test(projects.train.is.exciting.all, percent.train=percent.train)
  
  return(projects.train.is.exciting.all)
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

nb.days <- 350

is.exciting.eval <- make.gbm.train.model.estimate(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=0.1,
  n.trees=200
  )

# fully_funded.eval <- make.gbm.train.model.estimate(
#   variable="fully_funded",
#   days.hist=nb.days,
#   shrinkage=0.1,
#   n.trees=200
# )

# cat("auc fully_funded :",make.auc(fully_funded.eval), "\n")
cat("auc is_exciting :",make.auc(is.exciting.eval), "\n")

# is.exciting

is.exciting.eval.refined <- make.gbm.train.model.important(
  variable="is_exciting",
  days.hist=nb.days,
  shrinkage=0.005,
  n.trees=5000,
  model.cols=is.exciting.eval$important.cols
)

cat("auc is_exciting :",make.auc(is.exciting.eval.refined), "\n")


test.data <- make.projects.test(force=FALSE)

prediction <- predict(
  is.exciting.eval.refined$model, 
  newdata=test.data[,is.exciting.eval$important.cols],
  n.trees=5000, 
  type="response"
  )

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
