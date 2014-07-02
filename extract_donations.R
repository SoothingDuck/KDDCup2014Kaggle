source("variables.R")

library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

# Projects data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
projects.data <- dbGetQuery(
  con,
  "
  select
  T1.projectid as projectid,
  T1.teacher_acctid as teacher_acctid,
  T1.schoolid as schoolid,
  T1.school_ncesid as school_ncesid,
  T1.school_latitude as school_latitude,
  T1.school_longitude as school_longitude,
  T1.school_city as school_city,
  T1.school_state as school_state,
  T1.school_zip as school_zip,
  T1.school_metro as school_metro,
  T1.school_district as school_district,
  T1.school_county as school_county,
  T1.school_charter as school_charter,
  T1.school_magnet as school_magnet,
  T1.school_year_round as school_year_round,
  T1.school_nlns as school_nlns,
  T1.school_kipp as school_kipp,
  T1.school_charter_ready_promise as school_charter_ready_promise,
  T1.teacher_prefix as teacher_prefix,
  T1.teacher_teach_for_america as teacher_teach_for_america,
  T1.teacher_ny_teaching_fellow as teacher_ny_teaching_fellow,
  T1.primary_focus_subject as primary_focus_subject,
  T1.primary_focus_area as primary_focus_area,
  T1.secondary_focus_subject as secondary_focus_subject,
  T1.secondary_focus_area as secondary_focus_area,
  T1.resource_type as resource_type,
  T1.poverty_level as poverty_level,
  T1.grade_level as grade_level,
  -- T1.fulfillment_labor_materials as fulfillment_labor_materials,
  T1.total_price_excluding_optional_support as total_price_excluding_optional_support,
  T1.total_price_including_optional_support as total_price_including_optional_support,
  T1.students_reached as students_reached,
  T1.eligible_double_your_impact_match as eligible_double_your_impact_match,
  T1.eligible_almost_home_match as eligible_almost_home_match,
  T1.date_posted as date_posted,
  T2.typedataset as typedataset
  from projects T1 inner join project_dataset T2 on (T1.projectid=T2.projectid)
  "
)                                         
dbDisconnect(con)

# Donations data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
donations.data <- dbGetQuery(
  con,
  "
  select
  *
  from donations
  "
)                                         
dbDisconnect(con)
donations.data <- donations.data[, colnames(donations.data) != "row_names"]

# selection
library(lubridate)
projects.data$date_posted <- ymd(projects.data$date_posted)
projects.data$days_since_posted <- (as.integer(ymd("2014-05-12") - projects.data$date_posted)/(3600*24))
# projects.data <- subset(projects.data, days_since_posted <= 350)
# projects.data <- subset(projects.data, days_since_posted <= 180)
projects.data <- subset(projects.data, days_since_posted <= nb.days)

donations.data$donation_date <- ymd(substr(donations.data$donation_timestamp,1,10))
donations.data$days_since_donation <- (as.integer(ymd("2014-05-12") - donations.data$donation_date))
# donations.data <- subset(donations.data, days_since_donation <= 350)
# donations.data <- subset(donations.data, days_since_donation <= 180)
donations.data <- subset(donations.data, days_since_donation <= 1350)

# agg
library(plyr)

donations.by.person.agg <- ddply(
  donations.data,
  .(donor_acctid),
  summarise,
  
  total_donation_to_project=sum(donation_to_project),
  max_donation_to_project=max(donation_to_project),
  mean_donation_to_project=mean(donation_to_project),
  median_donation_to_project=median(donation_to_project),
  sd_donation_to_project=sd(donation_to_project),
  
  total_donation_optional_support=sum(donation_optional_support),
  max_donation_optional_support=max(donation_optional_support),
  mean_donation_optional_support=mean(donation_optional_support),
  median_donation_optional_support=median(donation_optional_support),
  sd_donation_optional_support=sd(donation_optional_support),
  
  total_donation_total=sum(donation_to_project+donation_optional_support),
  max_donation_total=max(donation_to_project+donation_optional_support),
  mean_donation_total=mean(donation_to_project+donation_optional_support),
  median_donation_total=median(donation_to_project+donation_optional_support),
  sd_donation_total=sd(donation_to_project+donation_optional_support),
  
  min_days_since_donation=min(days_since_donation),
  max_days_since_donation=max(days_since_donation),
  mean_days_since_donation=mean(days_since_donation),
  median_days_since_donation=mean(days_since_donation),
  sd_days_since_donation=sd(days_since_donation),
  
  nb_donation=length(donor_acctid)
)

donations.by.person.agg <- subset(donations.by.person.agg, mean_donation_optional_support < 100000)
donations.by.person.agg <- subset(donations.by.person.agg, nb_donation < 10000)

save(donations.by.person.agg, file=file.path("tmp","donations_by_person_agg.RData"))

# # semantic
# library(tm)
# 
# tmp <- ddply(
#   donations.data,
#   .(donor_acctid),
#   summarise,
#   donation_message_list=paste(donation_message, collapse=" ")
# )
# 
# 
# docs <- tmp$item_list
# names(docs) <- as.character(tmp$projectid)
# ds <- VectorSource(docs)
# 
# 
# print("generation corpus")
# corpus <- VCorpus(ds)
# 
# print("generatition dtm")
# corpus <- tm_map(corpus, removeNumbers)
# corpus <- tm_map(corpus, removePunctuation)
# # corpus <- tm_map(corpus, toupper)
# corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, stemDocument)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 
# dtm <- DocumentTermMatrix(corpus,
#                           control=list(
#                             weighting=weightTfIdf,
#                             stopwords=TRUE))
# 
# sparsed.dtm <- removeSparseTerms(dtm, 0.9)
# 
# sparsed.dtm.tmp <- inspect(sparsed.dtm)
# sparsed.dtm.tmp <- data.frame(sparsed.dtm.tmp)
# colnames(sparsed.dtm.tmp) <- paste("word", "item_name", colnames(sparsed.dtm.tmp), sep=".")
# 
# # for(col in colnames(sparsed.dtm.tmp)) {
# #   sparsed.dtm.tmp[, col] <- ifelse(sparsed.dtm.tmp[,col] > 0, 1, 0)
# # }
# 
# sparsed.dtm.tmp$projectid <- tmp$projectid
# 
# semantic.item_name.data <- sparsed.dtm.tmp
# 
# save(semantic.item_name.data, file=file.path("tmp","semantic_item_name.RData"))
# 
