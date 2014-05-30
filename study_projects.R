source("extract_projects.R")
source("extract_outcomes.R")

tmp <- merge(projects.data, outcomes.data, on=c("projectid"))

# Exciting project repartition
ggplot(tmp) + geom_histogram(aes(x=days_since_posted, fill=is_exciting))