library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

print("Extraction donnees outcomes...")

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

# Normalizae
outcomes.data$is_exciting <- factor(ifelse(outcomes.data$is_exciting == "t", "Yes", "No"))

outcomes.data$at_least_1_teacher_referred_donor[outcomes.data$at_least_1_teacher_referred_donor == ""] <- "Unknown"
outcomes.data$at_least_1_teacher_referred_donor[outcomes.data$at_least_1_teacher_referred_donor == "t"] <- "Yes"
outcomes.data$at_least_1_teacher_referred_donor[outcomes.data$at_least_1_teacher_referred_donor == "f"] <- "No"
outcomes.data$at_least_1_teacher_referred_donor <- factor(outcomes.data$at_least_1_teacher_referred_donor)

outcomes.data$fully_funded[outcomes.data$fully_funded == ""] <- "Unknown"
outcomes.data$fully_funded[outcomes.data$fully_funded == "t"] <- "Yes"
outcomes.data$fully_funded[outcomes.data$fully_funded == "f"] <- "No"
outcomes.data$fully_funded <- factor(outcomes.data$fully_funded)

outcomes.data$at_least_1_green_donation[outcomes.data$at_least_1_green_donation == ""] <- "Unknown"
outcomes.data$at_least_1_green_donation[outcomes.data$at_least_1_green_donation == "t"] <- "Yes"
outcomes.data$at_least_1_green_donation[outcomes.data$at_least_1_green_donation == "f"] <- "No"
outcomes.data$at_least_1_green_donation <- factor(outcomes.data$at_least_1_green_donation)

outcomes.data$three_or_more_non_teacher_referred_donors[outcomes.data$three_or_more_non_teacher_referred_donors == ""] <- "Unknown"
outcomes.data$three_or_more_non_teacher_referred_donors[outcomes.data$three_or_more_non_teacher_referred_donors == "t"] <- "Yes"
outcomes.data$three_or_more_non_teacher_referred_donors[outcomes.data$three_or_more_non_teacher_referred_donors == "f"] <- "No"
outcomes.data$three_or_more_non_teacher_referred_donors <- factor(outcomes.data$three_or_more_non_teacher_referred_donors)

outcomes.data$one_non_teacher_referred_donor_giving_100_plus[outcomes.data$one_non_teacher_referred_donor_giving_100_plus == ""] <- "Unknown"
outcomes.data$one_non_teacher_referred_donor_giving_100_plus[outcomes.data$one_non_teacher_referred_donor_giving_100_plus == "t"] <- "Yes"
outcomes.data$one_non_teacher_referred_donor_giving_100_plus[outcomes.data$one_non_teacher_referred_donor_giving_100_plus == "f"] <- "No"
outcomes.data$one_non_teacher_referred_donor_giving_100_plus <- factor(outcomes.data$one_non_teacher_referred_donor_giving_100_plus)

outcomes.data$donation_from_thoughtful_donor[outcomes.data$donation_from_thoughtful_donor == ""] <- "Unknown"
outcomes.data$donation_from_thoughtful_donor[outcomes.data$donation_from_thoughtful_donor == "t"] <- "Yes"
outcomes.data$donation_from_thoughtful_donor[outcomes.data$donation_from_thoughtful_donor == "f"] <- "No"
outcomes.data$donation_from_thoughtful_donor <- factor(outcomes.data$donation_from_thoughtful_donor)

# Nettoyage
rm(list=c("con", "drv", "sqlitedb.filename"))
