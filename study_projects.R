
projects.outcomes.filename <- file.path("tmp","projects_outcomes.RData")

if(! file.exists(projects.outcomes.filename)) {
  source("extract_projects.R")
  source("extract_outcomes.R")
  
  projects.outcomes.data <- merge(projects.data, outcomes.data, on=c("projectid"))
  save(projects.outcomes.data, file=file.path("tmp","projects_outcomes.RData"))
  
  rm(list=c("projects.data","outcomes.data"))
}

load(projects.outcomes.filename)

# Exciting project repartition
# ggplot(projects.outcomes.data) + geom_histogram(aes(x=days_since_posted, fill=is_exciting))

# test, train
indices <- sample(1:nrow(projects.outcomes.data), .3*nrow(projects.outcomes.data))
test.set <- projects.outcomes.data[indices,]
train.set <- projects.outcomes.data[-indices,]

variable.cible <- c(
  "is_exciting",
  "at_least_1_teacher_referred_donor",
  "fully_funded"
  )
model.list <- list()



xcols <- c(
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
"primary_focus_subject",
"primary_focus_area",
"resource_type",
"poverty_level",
"grade_level",
"fulfillment_labor_materials",
"students_reached",
"eligible_double_your_impact_match",
"eligible_almost_home_match",
"school_ncesid_status",
"total_price_excluding_optional_support",
"total_price_including_optional_support",
"days_since_posted",
"month_posted",
"day_of_week_posted"
)

ycol <- "is_exciting"

library(randomForest)
model.rf <- randomForest(
  x=train.set[,xcols],
  y=train.set[,ycol],
  xtest=test.set[,xcols],
  ytest=test.set[,ycol],
  importance=TRUE,
  do.trace=TRUE,
  proximity=FALSE,
  ntree=10
  )

importance.model.rf <- model.rf$importance
importance.No <- sort(importance.model.rf[,1], decreasing=TRUE)
importance.Yes <- sort(importance.model.rf[,2], decreasing=TRUE)


test.set$is_exciting_prediction <- predict(model.rf, newdata=test.set)
