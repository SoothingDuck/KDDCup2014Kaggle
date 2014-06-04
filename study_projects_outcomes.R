source("functions.R")

projects.outcomes.data <- get.projects.data(force=TRUE, with.outcomes=TRUE)

# test, train
indices <- sample(1:nrow(projects.outcomes.data), .3*nrow(projects.outcomes.data))
test.set <- projects.outcomes.data[indices,]
train.set <- projects.outcomes.data[-indices,]

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

trash.vars <- c(
  "teacher_referred_count",
  "non_teacher_referred_count",
  "great_messages_proportion",
  "typedataset",
  "projectid",
  "school_district",
  "school_zip",
  "school_city",
  "school_state",
  "school_ncesid",
  "teacher_acctid",
  "schoolid",
  "school_latitude",
  "school_longitude",
  "school_metro",
  "school_county",
  "secondary_focus_subject",
  "secondary_focus_area",
  "date_posted",
  "school_district_big",
  "school_ncesid_status"
  )

other.vars <- setdiff(names(projects.outcomes.data),variable.cible)
other.vars <- setdiff(other.vars, trash.vars)
xcols <- other.vars

model.list.projects.outcomes <- list()
model.list.projects.outcomes.filename <- file.path("tmp","model_random_forest_projects_outcomes.RData")

i <- 0
for(ycol in variable.cible) {
  i <- i + 1
  
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
  
  model.list.projects.outcomes[[ycol]] <- model.rf  
}

save(model.list.projects.outcomes, file=model.list.projects.outcomes.filename)
