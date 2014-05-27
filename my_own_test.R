library(RSQLite)

sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

# Essays data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
essay.data <- dbGetQuery(
  con,
  "
select
projectid,
length(title) as title_length,
length(short_description) as short_description_length,
length(need_statement) as need_statement_length,
length(essay) as essay_length
from essays
  "
)                                         
dbDisconnect(con)
essay.data <- essay.data[, colnames(essay.data) != "row_names"]

# Outcomes data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
outcomes.data <- dbGetQuery(
  con,
  "
select
*
from outcomes
  "
)                                         
dbDisconnect(con)
outcomes.data <- outcomes.data[, colnames(outcomes.data) != "row_names"]

# Project data set
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
set.data <- dbGetQuery(
  con,
  "
  select
  *
  from project_dataset
  "
)                                         
dbDisconnect(con)
set.data <- set.data[, colnames(set.data) != "row_names"]

# Projects data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
projects.data <- dbGetQuery(
  con,
  "
  select
  *
  from projects
  "
)                                         
dbDisconnect(con)
projects.data <- projects.data[, colnames(projects.data) != "row_names"]

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
