library(ROCR)
library(plyr)

auc <- function(y, predicted) {
  pred <- prediction(ifelse(predicted == "Yes",1,0), ifelse(y == "Yes",1,0))
  perf <- performance(pred, measure = "auc")
  return(attr(perf, "y.values")[[1]])
}

get.projects.data <- function(force=FALSE, with.outcomes=FALSE) {
  
  if(with.outcomes) {
    projects.filename <- file.path("tmp","projects_with_outcomes.RData")
    
    if((! file.exists(projects.filename)) | force) {
      source("extract_projects.R")
      source("extract_outcomes.R")
      
      projects.data <- merge(projects.data, outcomes.data, on=c("projectid"))
      
      save(projects.data, file=projects.filename)
    }
    
    load(projects.filename)
    
  } else {
    projects.filename <- file.path("tmp","projects_without_outcomes.RData")
    
    if((! file.exists(projects.filename)) | force) {
      source("extract_projects.R")
      
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
  
  return(projects.data)
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

get.categorical.vars <- function() {
  
  return(
    c(
      "school_state",
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
      "primary_focus_subject",
      "primary_focus_area",
      "secondary_focus_subject",
      "secondary_focus_area",
      "resource_type",
      "poverty_level",
      "grade_level",
      "fulfillment_labor_materials",
      "eligible_double_your_impact_match",
      "eligible_almost_home_match",
      "school_ncesid_status",
      "month_posted",
      "day_of_week_posted",
      "nb.distinct.school.by.ncesid"
      )
    )
  
}

get.numeric.vars <- function() {
  
  return(
    c(
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
      "nb.projects.by.county"
    )
  )
  
}

get.gbm.model <- function(xcols, ycol, ...) {
  xtrain <- get.projects.data.train(variable=ycol)
  ytrain <- xtrain[,ycol]
  xtrain <- xtrain[,xcols]
  
  model <- gbm.fit(x=xtrain, y=ytrain, verbose=TRUE, ...)
  
  return(model)
  
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
      projects.data <- projects.data[,setdiff(outcomes.cols, c(variable))]
      return(projects.data)
    } else {
      stop(variable)
    }
  }
  
}