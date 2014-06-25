source("functions.R")

model.list.projects.outcomes.filename <- file.path("tmp","model_random_forest_projects_outcomes.RData")
load(model.list.projects.outcomes.filename)

# Is_exciting
print(with(model.list.projects.outcomes$is_exciting, auc(y, predicted))) 

# at_least_1_teacher_referred_donor
print(with(model.list.projects.outcomes$at_least_1_teacher_referred_donor, auc(y, predicted))) 

# fully funded
print(with(model.list.projects.outcomes$fully_funded, auc(y, predicted))) 

# at_least_1_green_donation
print(with(model.list.projects.outcomes$at_least_1_green_donation, auc(y, predicted))) 

# great_chat
print(with(model.list.projects.outcomes$great_chat, auc(y, predicted))) 

# three_or_more_non_teacher_referred_donors
print(with(model.list.projects.outcomes$three_or_more_non_teacher_referred_donors, auc(y, predicted))) 

# one_non_teacher_referred_donor_giving_100_plus
print(with(model.list.projects.outcomes$one_non_teacher_referred_donor_giving_100_plus, auc(y, predicted))) 

# donation_from_thoughtful_donor
print(with(model.list.projects.outcomes$donation_from_thoughtful_donor, auc(y, predicted))) 
