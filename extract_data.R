
source("extract_essays.R")
source("extract_outcomes.R")
source("extract_resources.R")
source("extract_donations.R")
source("extract_projects.R")

# toute les données
all.data <- merge(set.data, essay.data, on=c("projectid"))
all.data <- merge(all.data, projects.data, on=c("projectid"))

# normalization
library(lubridate)

all.data$typedataset <- factor(all.data$typedataset)
all.data$school_ncesid_status <- factor(ifelse(is.na(all.data$school_ncesid), "NotAvailable", "Available"))
all.data$school_city_big <- factor(ifelse(
  all.data$school_city %in% names(which(prop.table(table(all.data$school_city)) > 0.01)),
  all.data$school_city,
  "SmallCity"
))

all.data$school_state <- factor(toupper(all.data$school_state))
all.data$school_metro <- factor(ifelse(all.data$school_metro == "", "Unknown", all.data$school_metro))
all.data$school_district_big <- factor(ifelse(
  all.data$school_district %in% names(which(prop.table(table(all.data$school_district)) > 0.01)),
  all.data$school_district,
  "SmallDistrict"
))

all.data$school_charter <- factor(ifelse(all.data$school_charter == "t", "Yes", "No"))
all.data$school_magnet <- factor(ifelse(all.data$school_magnet == "t", "Yes", "No"))
all.data$school_year_round <- factor(ifelse(all.data$school_year_round == "t", "Yes", "No"))
all.data$school_nlns <- factor(ifelse(all.data$school_nlns == "t", "Yes", "No"))
all.data$school_kipp <- factor(ifelse(all.data$school_kipp == "t", "Yes", "No"))
all.data$school_charter_ready_promise <- factor(ifelse(all.data$school_charter_ready_promise == "t", "Yes", "No"))

all.data$teacher_prefix[all.data$teacher_prefix == ""] <- "Mr." 
all.data$teacher_prefix[all.data$teacher_prefix == "Dr."] <- "Mr." 
all.data$teacher_prefix[all.data$teacher_prefix == "Mr. & Mrs."] <- "Mr." 
all.data$teacher_prefix <- factor(all.data$teacher_prefix)

all.data$teacher_teach_for_america <- factor(ifelse(all.data$teacher_teach_for_america == "t", "Yes", "No"))
all.data$teacher_ny_teaching_fellow <- factor(ifelse(all.data$teacher_ny_teaching_fellow == "t", "Yes", "No"))

all.data$primary_focus_subject[all.data$primary_focus_subject == ""] <- "Literacy"
all.data$primary_focus_subject <- factor(all.data$primary_focus_subject)

all.data$primary_focus_area[all.data$primary_focus_area == ""] <- "Literacy & Language"
all.data$primary_focus_area <- factor(all.data$primary_focus_area)

all.data$secondary_focus_subject <- factor(all.data$secondary_focus_subject)
all.data$secondary_focus_area <- factor(all.data$secondary_focus_area)

all.data$resource_type[all.data$resource_type == ""] <- "Supplies"
all.data$resource_type <- factor(all.data$resource_type)

all.data$poverty_level <- factor(all.data$poverty_level)

all.data$grade_level[all.data$grade_level == ""] <- "Grades PreK-2"
all.data$grade_level <- factor(all.data$grade_level)

all.data$students_reached <- ifelse(is.na(all.data$students_reached), 30.0, all.data$students_reached)

all.data$eligible_double_your_impact_match <- factor(ifelse(all.data$eligible_double_your_impact_match == "t", "Yes", "No"))
all.data$eligible_almost_home_match <- factor(ifelse(all.data$eligible_almost_home_match == "t", "Yes", "No"))

all.data$date_posted <- ymd(all.data$date_posted)
all.data$days_since_posted <- (as.integer(ymd("2014-05-12") - all.data$date_posted)/(3600*24))

all.data$month_posted <- factor(month(all.data$date_posted))
all.data$day_of_week_posted <- factor(weekdays(all.data$date_posted))

# séparation train et test
test.data <- subset(all.data, typedataset == "test")
train.data <- subset(all.data, typedataset == "train")

train.data <- merge(train.data, outcomes.data, on=c("projectid"))

# Ajustement train.data
train.data <- subset(train.data, ! is.na(fulfillment_labor_materials))

train.data$is_exciting <- factor(ifelse(train.data$is_exciting == "t", "Yes", "No"))

train.data$at_least_1_teacher_referred_donor[train.data$at_least_1_teacher_referred_donor == ""] <- "Unknown"
train.data$at_least_1_teacher_referred_donor[train.data$at_least_1_teacher_referred_donor == "t"] <- "Yes"
train.data$at_least_1_teacher_referred_donor[train.data$at_least_1_teacher_referred_donor == "f"] <- "No"
train.data$at_least_1_teacher_referred_donor <- factor(train.data$at_least_1_teacher_referred_donor)

train.data$fully_funded <- factor(ifelse(train.data$fully_funded == "t", "Yes", "No"))

train.data$at_least_1_green_donation[train.data$at_least_1_green_donation == ""] <- "Unknown"
train.data$at_least_1_green_donation[train.data$at_least_1_green_donation == "t"] <- "Yes"
train.data$at_least_1_green_donation[train.data$at_least_1_green_donation == "f"] <- "No"
train.data$at_least_1_green_donation <- factor(train.data$at_least_1_green_donation)

train.data$great_chat <- factor(ifelse(train.data$great_chat == "t", "Yes", "No"))

train.data$three_or_more_non_teacher_referred_donors[train.data$three_or_more_non_teacher_referred_donors == ""] <- "Unknown"
train.data$three_or_more_non_teacher_referred_donors[train.data$three_or_more_non_teacher_referred_donors == "t"] <- "Yes"
train.data$three_or_more_non_teacher_referred_donors[train.data$three_or_more_non_teacher_referred_donors == "f"] <- "No"
train.data$three_or_more_non_teacher_referred_donors <- factor(train.data$three_or_more_non_teacher_referred_donors)

train.data$one_non_teacher_referred_donor_giving_100_plus[train.data$one_non_teacher_referred_donor_giving_100_plus == ""] <- "Unknown"
train.data$one_non_teacher_referred_donor_giving_100_plus[train.data$one_non_teacher_referred_donor_giving_100_plus == "t"] <- "Yes"
train.data$one_non_teacher_referred_donor_giving_100_plus[train.data$one_non_teacher_referred_donor_giving_100_plus == "f"] <- "No"
train.data$one_non_teacher_referred_donor_giving_100_plus <- factor(train.data$one_non_teacher_referred_donor_giving_100_plus)

train.data$donation_from_thoughtful_donor[train.data$donation_from_thoughtful_donor == ""] <- "Unknown"
train.data$donation_from_thoughtful_donor[train.data$donation_from_thoughtful_donor == "t"] <- "Yes"
train.data$donation_from_thoughtful_donor[train.data$donation_from_thoughtful_donor == "f"] <- "No"
train.data$donation_from_thoughtful_donor <- factor(train.data$donation_from_thoughtful_donor)
