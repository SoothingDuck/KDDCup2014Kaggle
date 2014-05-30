
source("extract_essays.R")
source("extract_outcomes.R")
source("extract_resources.R")
source("extract_donations.R")
source("extract_projects.R")

# toute les données
all.data <- merge(set.data, essay.data, on=c("projectid"))
all.data <- merge(all.data, projects.data, on=c("projectid"))


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
