library(ROCR)
library(plyr)

auc <- function(y, predicted) {
  pred <- prediction(ifelse(predicted == "Yes",1,0), ifelse(y == "Yes",1,0))
  perf <- performance(pred, measure = "auc")
  return(attr(perf, "y.values")[[1]])
}

get.projects.data <- function(force=FALSE, with.outcomes=FALSE) {
  
  projects.filename <- file.path("tmp","projects.RData")
  
  if((! file.exists(projects.filename)) | force) {
    source("extract_projects.R")
  
    agg <- ddply(projects.data,
                 .(schoolid),
                 summarise,
                 nb.projects.for.school=length(schoolid))
    
    projects.data <- merge(projects.data, agg, on=c("schoolid"))

    agg <- ddply(projects.data,
                 .(teacher_acctid),
                 summarise,
                 nb.projects.for.teacher=length(teacher_acctid))

    projects.data <- merge(projects.data, agg, on=c("teacher_acctid"))
    
    agg <- ddply(subset(projects.data, ! is.na(school_ncesid)),
                 .(school_ncesid),
                 summarise,
                 nb.distinct.school.by.ncesid=length(unique(schoolid))
    )
    
    projects.data <- merge(projects.data, agg, on=c("school_ncesid"), all.x = TRUE)
    projects.data$nb.distinct.school.by.ncesid <- with(projects.data, factor(ifelse(is.na(nb.distinct.school.by.ncesid), 1, nb.distinct.school.by.ncesid)))
    
    agg <- ddply(projects.data,
                 .(school_state),
                 summarise,
                 nb.projects.by.state=length(school_state)
    )

    projects.data <- merge(projects.data, agg, on=c("school_state"))
    
    agg <- ddply(projects.data,
                 .(school_city),
                 summarise,
                 nb.projects.by.city=length(school_city)
    )

    projects.data <- merge(projects.data, agg, on=c("school_city"))

    projects.data <- subset(projects.data, ! is.na(school_zip))
    agg <- ddply(projects.data,
                 .(school_zip),
                 summarise,
                 nb.projects.by.zip=length(school_zip)
    )
    
    projects.data <- merge(projects.data, agg, on=c("school_zip"))
    
    agg <- ddply(projects.data,
                 .(school_district),
                 summarise,
                 nb.projects.by.district=length(school_district)
    )
    
    projects.data <- merge(projects.data, agg, on=c("school_district"))
    
    save(projects.data, file=projects.filename)
  }
  
  load(projects.filename)
  
  if(with.outcomes) {
    source("extract_outcomes.R")
    
    projects.data <- merge(projects.data, outcomes.data, on=c("projectid"))
  }
  
  return(projects.data)
}