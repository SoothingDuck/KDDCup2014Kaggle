
projects.outcomes.essays.filename <- file.path("tmp","projects_outcomes_essays.RData")

if(! file.exists(projects.outcomes.essays.filename)) {
  source("extract_projects.R")
  source("extract_outcomes.R")
  source("extract_essays.R")
  
  projects.outcomes.essays.data <- merge(projects.data, outcomes.data, on=c("projectid"))
  projects.outcomes.essays.data <- merge(projects.outcomes.essays.data, essays.data, on=c("projectid"))
  save(projects.outcomes.essays.data, file=projects.outcomes.essays.filename)
  
  rm(list=c("projects.data","outcomes.data","essays.data"))
}

load(projects.outcomes.essays.filename)

# Exciting project repartition
# ggplot(projects.outcomes.data) + geom_histogram(aes(x=days_since_posted, fill=is_exciting))

# test, train
indices <- sample(1:nrow(projects.outcomes.essays.data), .3*nrow(projects.outcomes.essays.data))
test.set <- projects.outcomes.essays.data[indices,]
train.set <- projects.outcomes.essays.data[-indices,]

variable.cible <- c(
  "is_exciting",
  "at_least_1_teacher_referred_donor",
  "fully_funded",
  "at_least_1_green_donation",
  "great_chat",
  "three_or_more_non_teacher_referred_donors",
  "one_non_teacher_referred_donor_giving_100_plus",
  "donation_from_thoughtful_donor"
  )

model.list.projects.outcomes.essays <- list()
model.list.projects.outcomes.essays.filename <- file.path("tmp","model_random_forest_projects_outcomes_essays.RData")

i <- 0
for(ycol in variable.cible) {
  i <- i + 1
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
    "day_of_week_posted",
    "title_length",
    "short_description_length",
    "need_statement_length",
    "essay_length"
  )
  
  tmp.train <- train.set
  tmp.test <- test.set
  
  if("Unknown" %in% levels(tmp.train[,ycol])) {
    tmp.train <- tmp.train[tmp.train[,ycol] != "Unknown",]
    tmp.train[,ycol] <- factor(tmp.train[,ycol])
    tmp.test <- tmp.test[tmp.test[,ycol] != "Unknown",]
    tmp.test[,ycol] <- factor(tmp.test[,ycol])
  }
  
  library(randomForest)
  model.rf <- randomForest(
    x=tmp.train[,xcols],
    y=tmp.train[,ycol],
    xtest=tmp.test[,xcols],
    ytest=tmp.test[,ycol],
    importance=TRUE,
    do.trace=TRUE,
    proximity=FALSE,
    keep.forest=TRUE,
    ntree=50
  )
  
  model.list.projects.outcomes.essays[[ycol]] <- model.rf  
}

save(model.list.projects.outcomes.essays, file=model.list.projects.outcomes.essays.filename)
