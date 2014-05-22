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

# outcomes.data <- subset(outcomes.data, at_least_1_teacher_referred_donor != "")
# outcomes.data <- subset(outcomes.data, at_least_1_green_donation != "")
# outcomes.data <- subset(outcomes.data, one_non_teacher_referred_donor_giving_100_plus != "")
# outcomes.data <- subset(outcomes.data, donation_from_thoughtful_donor != "")
# outcomes.data <- subset(outcomes.data, fully_funded != "")

outcomes.data$is_exciting <- factor(ifelse(outcomes.data$is_exciting == "t", "Yes", "No"))
outcomes.data$at_least_1_teacher_referred_donor <- factor(ifelse(outcomes.data$at_least_1_teacher_referred_donor == "t", "Yes", "No"))
outcomes.data$fully_funded <- factor(ifelse(outcomes.data$fully_funded == "t", "Yes", "No"))
outcomes.data$at_least_1_green_donation <- factor(ifelse(outcomes.data$at_least_1_green_donation == "t", "Yes", "No"))
outcomes.data$great_chat <- factor(ifelse(outcomes.data$great_chat == "t", "Yes", "No"))
outcomes.data$three_or_more_non_teacher_referred_donors <- factor(ifelse(outcomes.data$three_or_more_non_teacher_referred_donors == "t", "Yes", "No"))
outcomes.data$one_non_teacher_referred_donor_giving_100_plus <- factor(ifelse(outcomes.data$one_non_teacher_referred_donor_giving_100_plus == "t", "Yes", "No"))
outcomes.data$donation_from_thoughtful_donor <- factor(ifelse(outcomes.data$donation_from_thoughtful_donor == "t", "Yes", "No"))

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

# train and test set
all.data <- merge(set.data, essay.data, on=c("projectid"))
all.data <- merge(all.data, projects.data, on=c("projectid"))

test.data <- subset(all.data, typedataset == "test")
train.data <- subset(all.data, typedataset == "train")

train.data <- merge(train.data, outcomes.data, on=c("projectid"))
