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
  
  projects.data <- merge(projects.data, essays.data, by=c("projectid"), all.x=TRUE)
  projects.data <- merge(projects.data, resources.by.type, by=c("projectid"), all.x=TRUE)
  
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
  tmp <- union(tmp, colnames(data)[grepl("primary_focus_subject", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("secondary_focus_subject", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("primary_focus_area", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("secondary_focus_area", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("school_district_restriction", colnames(data))])
  # tmp <- union(tmp, colnames(data)[grepl("school_county_restriction", colnames(data))])
  
  tmp <- union(tmp, c(
    # "school_state",
    "school_metro",
    "school_charter",
    "school_magnet",
    "school_year_round",
    "school_nlns",
    "school_kipp",
    "school_charter_ready_promise",
    "teacher_prefix",
    "teacher_teach_for_america",
    "teacher_ny_teaching_fellow",
    # "primary_focus_subject",
    # "primary_focus_area",
    # "secondary_focus_subject",
    # "secondary_focus_area",
    "resource_type",
    "poverty_level",
    "grade_level",
    "fulfillment_labor_materials",
    "eligible_double_your_impact_match",
    "eligible_almost_home_match",
    "school_ncesid_status",
    "month_posted",
    "year_posted",
    "day_of_week_posted",
    "nb.distinct.school.by.ncesid",
    "total_price_excluding_optional_support",
    "total_price_including_optional_support",
    "students_reached",
    "days_since_posted",
    "nb.projects.for.school",
    "nb.projects.for.teacher",
    "nb.projects.by.state",
    "nb.projects.by.city",
    "nb.projects.by.zip",
    "nb.projects.by.district",
    "nb.projects.by.county",
    # "school_district_factor",
    "total_price_optional_support"
  )
  )
  
  return(tmp)
  
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
    "nb_distinct_vendors_project"
    )
  
  tmp <- union(tmp, colnames(data)[grepl("_total_price_resource", colnames(data))])
  tmp <- union(tmp, colnames(data)[grepl("_nb_item_resource", colnames(data))])
  
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
