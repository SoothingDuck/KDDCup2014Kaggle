source("functions.R")

projects.data <- get.projects.data(force=TRUE)

library(plyr)
library(ggplot2)

# Nombre de projects distincts par ecole
agg <- ddply(projects.data,
             .(schoolid, typedataset),
             summarise,
             count=length(schoolid))

ggplot(agg) + geom_boxplot(aes(x=typedataset, y=count))

# Nombre de projects distincts par professeur
agg <- ddply(projects.data,
             .(teacher_acctid, typedataset),
             summarise,
             count=length(teacher_acctid))

ggplot(agg) + geom_boxplot(aes(x=typedataset, y=count))

# Nombre d'écoles distinctes par school_ncesid
agg <- ddply(subset(projects.data, ! is.na(school_ncesid)),
             .(school_ncesid, typedataset),
             summarise,
             count=length(unique(schoolid))
)

ggplot(agg) + geom_boxplot(aes(x=typedataset, y=count))

# Project localisation
ggplot(projects.data) + geom_point(aes(x=school_longitude, y=school_latitude, color=school_state)) +  facet_wrap(~ typedataset)

test <- subset(projects.data, school_latitude > 50) # Terres australes
test2 <- subset(projects.data, school_latitude < 25 & school_longitude < -150)

# NB projects by state
agg <- ddply(projects.data,
              .(school_state, typedataset),
              summarise,
              count=length(school_state)
)

ggplot(agg) + geom_bar(aes(x=school_state,y=count), stat="identity") + facet_wrap(~ typedataset) +
  theme(axis.text.x = element_text(angle = 90))

# NB projects by city
agg <- ddply(projects.data,
             .(school_city, typedataset),
             summarise,
             count=length(school_city)
)

# Nb projects by School zip
agg <- ddply(projects.data,
             .(school_zip, typedataset),
             summarise,
             count=length(school_zip))

ggplot(agg) + geom_density(aes(x=count, fill=typedataset))

# Nb projects by School district
agg <- ddply(projects.data,
             .(school_district, typedataset),
             summarise,
             count=length(school_district))

ggplot(agg) + geom_boxplot(aes(x=typedataset, y=count))

