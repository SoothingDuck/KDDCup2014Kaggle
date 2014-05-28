source("extract_data.R")

# sample
indices.train <- sample(1:nrow(train.data), 10000)
train.data.sample <- train.data[indices.train,]

# Menage
# rm(list=c("all.data", "essay.data","outcomes.data", "projects.data", "set.data"))
# gc(TRUE)

# Naive is_exciting
library(gbm)
library(randomForest)

model_1_rf_is_exciting <- randomForest(
  is_exciting ~
    title_length +
    short_description_length +
    need_statement_length +
    essay_length +
    # school_state +
    school_metro +
    school_charter +
    school_magnet +
    school_year_round +
    school_nlns +
    school_kipp +
    school_charter_ready_promise +
    teacher_prefix +
    teacher_teach_for_america +
    teacher_ny_teaching_fellow +
    primary_focus_subject +
    # secondary_focus_subject +
    # primary_focus_area +
    # secondary_focus_area +
    resource_type+
    poverty_level+
    grade_level +
    fulfillment_labor_materials +
    total_price_excluding_optional_support +
    total_price_including_optional_support +
    students_reached +
    eligible_double_your_impact_match +
    eligible_almost_home_match +
    school_ncesid_status +
    #school_city_big +
    #school_district_big +
    days_since_posted +
    month_posted +
    day_of_week_posted,
  data=train.data.sample,
  ntree=200,
  importance=TRUE,
  do.trace=TRUE
  )

model_1_rf_at_least_1_teacher_referred_donor <- randomForest(
  I(factor(at_least_1_teacher_referred_donor)) ~
    title_length +
    short_description_length +
    need_statement_length +
    essay_length +
    # school_state +
    school_metro +
    school_charter +
    school_magnet +
    school_year_round +
    school_nlns +
    school_kipp +
    school_charter_ready_promise +
    teacher_prefix +
    teacher_teach_for_america +
    teacher_ny_teaching_fellow +
    primary_focus_subject:secondary_focus_subject +
    # secondary_focus_subject +
    primary_focus_area +
    # secondary_focus_area +
    resource_type+
    poverty_level+
    grade_level +
    fulfillment_labor_materials +
    total_price_excluding_optional_support +
    total_price_including_optional_support +
    I(total_price_including_optional_support-total_price_excluding_optional_support) +
    students_reached +
    eligible_double_your_impact_match +
    eligible_almost_home_match +
    school_ncesid_status +
    #school_city_big +
    #school_district_big +
    days_since_posted +
    month_posted +
    day_of_week_posted,
  data=subset(train.data.sample, at_least_1_teacher_referred_donor != "Unknown"),
  ntree=500,
  importance=TRUE,
  do.trace=TRUE
)


