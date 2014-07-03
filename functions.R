library(ROCR)
library(plyr)
library(gbm)
library(randomForest)

auc <- function(y, predicted) {
  pred <- prediction(predictions=predicted, labels=ifelse(y == "Yes",1,0))
  perf <- performance(pred, measure = "auc")
  return(attr(perf, "y.values")[[1]])
}


init.data <- function() {
  source("extract_projects.R")
  source("extract_essays.R")
  source("extract_resources.R")
  
  print("extraction des essays...")
  projects.data <- merge(projects.data, essays.data, by=c("projectid"), all.x=TRUE)
  print("extraction des ressources...")
  projects.data <- merge(projects.data, resources.by.type, by=c("projectid"), all.x=TRUE)
  projects.data <- merge(projects.data, resources.vendor, by=c("projectid"), all.x=TRUE)
  
  for(col in names(projects.data)[grepl("_resource", names(projects.data))]) {
    projects.data[, col] <- ifelse(is.na(projects.data[, col]), 0, projects.data[, col])
  }
  
  for(col in c("total_price_project", "nb_item_project", "nb_distinct_vendors_project")) {
    projects.data[, col] <- ifelse(is.na(projects.data[, col]), 0, projects.data[, col])    
  }
  
  return(projects.data)
}

get.projects.data <- function(force=FALSE, with.outcomes=FALSE) {
  
  if(with.outcomes) {
    projects.filename <- file.path("tmp","projects_with_outcomes.RData")
    
    if((! file.exists(projects.filename)) | force) {
      projects.data <- init.data()
      
      source("extract_outcomes.R")
      projects.data <- merge(projects.data, outcomes.data, on=c("projectid"))
            
      save(projects.data, file=projects.filename)
    }
    
    load(projects.filename)
    
  } else {
    projects.filename <- file.path("tmp","projects_without_outcomes.RData")
    
    if((! file.exists(projects.filename)) | force) {
      projects.data <- init.data()
      
      save(projects.data, file=projects.filename)
    }
    
    load(projects.filename)
  }
  
  projects.data <- subset(projects.data, ! is.na(total_price_project))
  
  return(projects.data)
}

get.projects.data.test <- function(force=FALSE) {
  
  projects.data <- get.projects.data(force=force)
  projects.data <- subset(projects.data, typedataset == "test")
  
  projects.data <- projects.data[,colnames(projects.data) != "typedataset"]
  
  for(col in names(projects.data)[grepl("_total_price_resource", names(projects.data))]) {
    projects.data[is.na(projects.data[,col]),col] <- 0
  }
  
  for(col in names(projects.data)[grepl("_nb_item_resource", names(projects.data))]) {
    projects.data[is.na(projects.data[,col]),col] <- 0
  }

  for(col in c("total_price_project","nb_item_project","nb_distinct_vendors_project")) {
    projects.data[is.na(projects.data[,col]),col] <- 0
  }
  
  return(projects.data)
}

get.projects.data.train <- function(force=FALSE, variable=NULL) {
  
  projects.data <- get.projects.data(force=force, with.outcomes=TRUE)
  projects.data <- subset(projects.data, typedataset == "train")
  projects.data <- projects.data[,colnames(projects.data) != "typedataset"]
  
  outcomes.cols <- get.train.columns()
  
  if(is.null(variable)) {
    return(projects.data)    
  } else {
    if(variable %in% outcomes.cols) {
      projects.data <- projects.data[! is.na(projects.data[,c(variable)]),]
      projects.data <- projects.data[,setdiff(names(projects.data),setdiff(outcomes.cols, c(variable)))]
      return(projects.data)
    } else {
      stop(variable)
    }
  }
  
}


get.train.columns <- function() {
  return(c(
    "is_exciting",
    "at_least_1_teacher_referred_donor",
    "fully_funded",
    "at_least_1_green_donation",
    "great_chat",
    "three_or_more_non_teacher_referred_donors",
    "one_non_teacher_referred_donor_giving_100_plus",
    "donation_from_thoughtful_donor",
    "great_messages_proportion",
    "teacher_referred_count",
    "non_teacher_referred_count"    
  ))
}

get.project.variables <- function(data) {
  
  tmp <- c()
  
  tmp <- union(tmp, colnames(data)[grepl("school_state", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("teacher_prefix", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("school_metro", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("resource_type", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("poverty_level", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("primary_focus_subject", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("secondary_focus_subject", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("primary_focus_area", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("secondary_focus_area", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("school_district_restriction", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("school_county_restriction", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("primary_focus_merge", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("school_city_big", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("school_district_big", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("month_posted", colnames(data))])
  
  tmp <- tmp[! grepl("primary_focus_merge", tmp)]
  
  tmp <- union(tmp, c(
    # "school_state",
    # "school_metro",
    "school_charter",
    "school_magnet",
    "school_year_round",
    "school_nlns",
    "school_kipp",
    "school_charter_ready_promise",
    # "teacher_prefix",
    "teacher_teach_for_americaYes",
    "teacher_ny_teaching_fellow",
    # "primary_focus_subject",
    # "primary_focus_area",
    # "secondary_focus_subject",
    # "secondary_focus_area",
    # "resource_type",
    # "poverty_level",
    # "grade_level",
    # "fulfillment_labor_materials",
    "eligible_double_your_impact_match",
    "eligible_almost_home_match",
    # "school_ncesid_status",
    # "month_posted",
    # "year_posted",
    # "day_of_week_posted",
    # "nb.distinct.school.by.ncesid",
    "total_price_excluding_optional_support",
    "total_price_including_optional_support",
    "students_reached",
    "days_since_posted",
    "months_since_posted",
    "weeks_since_posted",
    "count.weeks.since.posted",
    "nb.projects.for.school",
    "nb.projects.for.teacher",
    # "nb.projects.by.state",
    "nb.projects.by.city",
    "nb.projects.by.zip",
    # "nb.projects.by.district",
    "nb.projects.by.county",
    # "school_district_factor",
    "total_price_optional_support"
  )
  )
  
  return(tmp)
  
}


make.sub.model.matrix <- function(data, formula, prefix, nb.up) {
  
  
  if(! any(grepl(":", as.character(formula)))) {
    var <- as.character(formula)[2]
    data[, var] <-  as.character(data[, var])
    t <- data.frame(table(data[, var]))
    t <- t[order(-t$Freq),]
    t$Var1 <- as.character(t$Var1)
    
    data[! data[, var] %in% t$Var1[1:nb.up], var] <- "Poubelle"
  }
  
  t <- data.frame(model.matrix(formula, data=data))
  t <- t[,2:ncol(t)]
  
  if(! any(grepl(":", as.character(formula)))) {
    var <- as.character(formula)[2]
    t <- t[, colnames(t) != paste(var, "Poubelle", sep="")]
    names(t) <- paste(prefix, names(t), sep=".")
    t$projectid <- projects.data$projectid
    return(t)
  } else {
    t$projectid <- projects.data$projectid
    m <- melt(t, id.vars="projectid")
    rm(list="t")
    gc(TRUE)
    u <- subset(m, value > 0)
    s <- data.frame(table(u$variable))
    s <- s[order(-s$Freq),]
    s.list <- s$Var1[1:nb.up]
    m <- subset(m, variable %in% s.list)
    v <- dcast(m, projectid ~ variable)
    names(v) <- c("projectid", paste(prefix, names(v[2:ncol(v)]), sep="."))
    return(v)    
  }
}


get.essay.variables <- function() {
  
  return(
    c(
      "title_length",
      "short_description_length",
      "need_statement_length",
      "essay_length"
    )
  )
  
}

get.resource.variables <- function(data) {
  
  tmp <- c(
    "total_price_project",
    "nb_item_project",
    "nb_distinct_vendors_project",
    "count.distinct.vendors"
    )
  
  tmp <- union(tmp, colnames(data)[grepl("_total_price_resource", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("_nb_item_resource", colnames(data))])
  
  tmp <- union(tmp, colnames(data)[grepl("vendor_name.", colnames(data))])  
  
  return(tmp)
  
}

get.all.variables <- function(data) {

  vars <- c()
  
  vars <- union(vars, get.project.variables(data))
  vars <- union(vars, get.essay.variables())
  vars <- union(vars, get.resource.variables(data))
  
  return(vars)
  
}


get.gbm.model <- function(xtrain, ytrain, ...) {
  ytrain <- ifelse(ytrain == "Yes", 1.0, 0.0)
  
  model <- gbm.fit(x=xtrain, y=ytrain, verbose=TRUE, ...)
  
  return(model)
  
}

split.train.test <- function(data, percent.train=.7) {
  indices.train <- sample(1:nrow(data), size=nrow(data)*percent.train)
  
  train.data <- data[indices.train,]
  test.data <- data[-indices.train,]
  
  return(list(train=train.data, test=test.data))
}


make.auc <- function(model.object, step.trees=20) {
  
  y <- model.object$data.test[,model.object$y.variable]
  
  model.cols <- rownames(model.object$importance)
  
  result <- data.frame()
  for(n.trees in c(seq(1, model.object$n.trees, step.trees),model.object$n.trees)) {
    predicted <- predict(model.object$model, newdata=model.object$data.test[,model.cols],n.trees=n.trees, type="response")
    auc.value <- auc(y, predicted)
    
    result <- rbind(
      result,
      data.frame(
        n.tree=n.trees,
        auc=auc.value
        )
      )
  }
  
  return(result)
}

make.model.variable.list <- function(data) {
  
  all.cols <- get.all.variables(data)
  all.cols <- union(all.cols, colnames(data)[grepl("word", colnames(data))])
  
  all.cols <- union(all.cols, c("count.word"))
  
    #   total_donation_to_project=sum(donation_to_project),
    #   max_donation_to_project=max(donation_to_project),
    #   mean_donation_to_project=mean(donation_to_project),
    #   median_donation_to_project=median(donation_to_project),
    #   sd_donation_to_project=median(donation_to_project),
    all.cols <- union(
      all.cols,
      c(
        "total_donation_to_project", 
        "max_donation_to_project",
        "mean_donation_to_project",
        "median_donation_to_project",
        "sd_donation_to_project"
      )
    )
    
    #   total_donation_optional_support=sum(donation_optional_support),
    #   max_donation_optional_support=max(donation_optional_support),
    #   mean_donation_optional_support=mean(donation_optional_support),
    #   median_donation_optional_support=median(donation_optional_support),
    #   sd_donation_optional_support=median(donation_optional_support),
    all.cols <- union(
      all.cols, 
      c(
        "total_donation_optional_support", 
        "max_donation_optional_support",
        "mean_donation_optional_support",
        "median_donation_optional_support",
        "sd_donation_optional_support"
      )
    )
    
    #   total_donation_total=sum(donation_to_project+donation_optional_support),
    #   max_donation_total=max(donation_to_project+donation_optional_support),
    #   mean_donation_total=mean(donation_to_project+donation_optional_support),
    #   median_donation_total=median(donation_to_project+donation_optional_support),
    #   sd_donation_total=median(donation_to_project+donation_optional_support),
    all.cols <- union(
      all.cols, 
      c(
        "total_donation_total", 
        "max_donation_total",
        "mean_donation_total",
        "median_donation_total",
        "sd_donation_total"
      )
    )
    
    
    #   min_days_since_donation=min(days_since_donation),
    #   max_days_since_donation=max(days_since_donation),
    #   mean_days_since_donation=mean(days_since_donation),
    #   median_days_since_donation=median(days_since_donation)  
    #   sd_days_since_donation=median(days_since_donation)  
    all.cols <- union(
      all.cols, 
      c(
        "min_days_since_donation", 
        "max_days_since_donation",
        "mean_days_since_donation",
        "median_days_since_donation",
        "sd_days_since_donation",
	"mean_diff_days_since_donation",
	"median_diff_days_since_donation"
      ))
    
    #   nb_donation=length(donor_acctid)
    all.cols <- union(
      all.cols, 
      c(
        "nb_donation",
        "is_teacher_donator"
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
  
  data <- merge(data, semantic.item_name.data, by="projectid", all.x=TRUE)
  data <- merge(data, semantic.short_description.data, by="projectid", all.x=TRUE)
  data <- merge(data, semantic.title.data, by="projectid", all.x=TRUE)
  data <- merge(data, semantic.essay.data, by="projectid", all.x=TRUE)
  data <- merge(data, semantic.need_statement.data, by="projectid", all.x=TRUE)
  data <- merge(data, count.essay.data, by="projectid", all.x=TRUE)
  
  for(col in names(data)[grepl("word.", names(data))]) {
    data[, col] <- ifelse(is.na(data[, col]), 0, data[, col])
  }
  
  data[,"count.word"] <- ifelse(is.na(data[, "count.word"]), 0, data[, "count.word"])
  
  data <- merge(data, donations.by.person.agg, by.x="teacher_acctid", by.y="donor_acctid", all.x=TRUE)
  
  data$is_teacher_donator <- with(data, ifelse(is.na(total_donation_to_project), 0, 1))
  
  data$total_donation_to_project <- with(data, ifelse(is.na(total_donation_to_project), 0, total_donation_to_project))
  data$median_donation_to_project <- with(data, ifelse(is.na(median_donation_to_project), 0, median_donation_to_project))
  data$max_donation_to_project <- with(data, ifelse(is.na(max_donation_to_project), 0, max_donation_to_project))
  data$mean_donation_to_project <- with(data, ifelse(is.na(mean_donation_to_project), 0, mean_donation_to_project))
  data$sd_donation_to_project <- with(data, ifelse(is.na(sd_donation_to_project), 0, sd_donation_to_project))
  
  data$total_donation_optional_support <- with(data, ifelse(is.na(total_donation_optional_support), 0, total_donation_optional_support))
  data$max_donation_optional_support <- with(data, ifelse(is.na(max_donation_optional_support), 0, max_donation_optional_support))
  data$mean_donation_optional_support <- with(data, ifelse(is.na(mean_donation_optional_support), 0, mean_donation_optional_support))
  data$median_donation_optional_support <- with(data, ifelse(is.na(median_donation_optional_support), 0, median_donation_optional_support))
  data$sd_donation_optional_support <- with(data, ifelse(is.na(sd_donation_optional_support), 0, sd_donation_optional_support))
  
  data$total_donation_total <- with(data, ifelse(is.na(total_donation_total), 0, total_donation_total))
  data$max_donation_total <- with(data, ifelse(is.na(max_donation_total), 0, max_donation_total))
  data$mean_donation_total <- with(data, ifelse(is.na(mean_donation_total), 0, mean_donation_total))
  data$median_donation_total <- with(data, ifelse(is.na(median_donation_total), 0, median_donation_total))
  data$sd_donation_total <- with(data, ifelse(is.na(sd_donation_total), 0, sd_donation_total))
  
  data$min_days_since_donation <- with(data, ifelse(is.na(min_days_since_donation), 5000, min_days_since_donation))
  data$max_days_since_donation <- with(data, ifelse(is.na(max_days_since_donation), 5000, max_days_since_donation))
  data$mean_days_since_donation <- with(data, ifelse(is.na(mean_days_since_donation), 5000, mean_days_since_donation))
  data$median_days_since_donation <- with(data, ifelse(is.na(median_days_since_donation), 5000, median_days_since_donation))
  data$sd_days_since_donation <- with(data, ifelse(is.na(sd_days_since_donation), 0, sd_days_since_donation))
  data$mean_diff_days_since_donation <- with(data, ifelse(is.na(mean_diff_days_since_donation), 0, mean_diff_days_since_donation))
  data$median_diff_days_since_donation <- with(data, ifelse(is.na(median_diff_days_since_donation), 0, median_diff_days_since_donation))
  
  data$nb_donation <- with(data, ifelse(is.na(nb_donation), 0, nb_donation))
      
  return(data)
}

make.projects.train <-  function(variable, days.hist, force=FALSE, percent.train=0.95) {
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

make.gbm.train.model.estimate <- function(variable, days.hist, shrinkage, n.trees, force=FALSE, percent.train=0.95) {
  
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
