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