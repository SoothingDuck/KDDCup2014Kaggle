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

donations.data$donor_city[donations.data$donor_city == ""] <- "Unknown"
donations.data$is_teacher_acct <- with(donations.data, ifelse(is_teacher_acct == 't', 1, 0))

tmp <- model.matrix(~ dollar_amount - 1, data=donations.data)
donations.data <- cbind(donations.data, tmp)

donations.data$donation_included_optional_support <- with(donations.data, ifelse(donation_included_optional_support == 't', 1, 0))

tmp <- model.matrix(~ payment_method - 1, data=donations.data)
donations.data <- cbind(donations.data, tmp)

donations.data$payment_included_acct_credit <- with(donations.data, ifelse(payment_included_acct_credit == 't', 1, 0))
donations.data$payment_included_campaign_gift_card <- with(donations.data, ifelse(payment_included_campaign_gift_card == 't', 1, 0))
donations.data$payment_included_web_purchased_gift_card <- with(donations.data, ifelse(payment_included_web_purchased_gift_card == 't', 1, 0))
donations.data$payment_was_promo_matched <- with(donations.data, ifelse(payment_was_promo_matched == 't', 1, 0))
donations.data$via_giving_page <- with(donations.data, ifelse(via_giving_page == 't', 1, 0))
donations.data$for_honoree <- with(donations.data, ifelse(for_honoree == 't', 1, 0))



# selection
library(lubridate)
projects.data$date_posted <- ymd(projects.data$date_posted)
projects.data$days_since_posted <- (as.integer(ymd("2014-05-12") - projects.data$date_posted)/(3600*24))
# projects.data <- subset(projects.data, days_since_posted <= 350)
# projects.data <- subset(projects.data, days_since_posted <= 180)
projects.data <- subset(projects.data, days_since_posted <= 350)

donations.data$donation_date <- ymd(substr(donations.data$donation_timestamp,1,10))
donations.data$days_since_donation <- (as.integer(ymd("2014-05-12") - donations.data$donation_date))
# donations.data <- subset(donations.data, days_since_donation <= 350)
# donations.data <- subset(donations.data, days_since_donation <= 180)
donations.data <- subset(donations.data, days_since_donation <= 350)

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
  
  total_donation_optional_support=sum(donation_optional_support),
  max_donation_optional_support=max(donation_optional_support),
  mean_donation_optional_support=mean(donation_optional_support),
  median_donation_optional_support=median(donation_optional_support),
  
  total_donation_total=sum(donation_to_project+donation_optional_support),
  max_donation_total=max(donation_to_project+donation_optional_support),
  mean_donation_total=mean(donation_to_project+donation_optional_support),
  median_donation_total=median(donation_to_project+donation_optional_support),
  
  min_days_since_donation=min(days_since_donation),
  max_days_since_donation=max(days_since_donation),
  mean_days_since_donation=mean(days_since_donation),
  median_days_since_donation=median(days_since_donation),
  
  nb_donation=length(donor_acctid),
  
  nb_donation_included_optional_support=sum(donation_included_optional_support),
  nb_payment_included_acct_credit=sum(payment_included_acct_credit),
  nb_payment_included_campaign_gift_card=sum(payment_included_campaign_gift_card),
  nb_payment_included_web_purchased_gift_card=sum(payment_included_web_purchased_gift_card),
  nb_payment_was_promo_matched=sum(payment_was_promo_matched),
  nb_via_giving_page=sum(via_giving_page),
  nb_for_honoree=sum(for_honoree),
  nb_dollar_amount10_to_100=sum(dollar_amount10_to_100),
  nb_dollar_amount100_and_up=sum(dollar_amount100_and_up),
  nb_dollar_amountunder_10=sum(dollar_amountunder_10),
  nb_payment_methodalmost_home_match=sum(payment_methodalmost_home_match),
  nb_payment_methodamazon=sum(payment_methodamazon),
  nb_payment_methodcheck=sum(payment_methodcheck),
  nb_payment_methodcreditcard=sum(payment_methodcreditcard),
  nb_payment_methoddouble_your_impact_match=sum(payment_methoddouble_your_impact_match),
  nb_payment_methodno_cash_received=sum(payment_methodno_cash_received),
  nb_payment_methodpaypal=sum(payment_methodpaypal),
  nb_payment_methodpromo_code_match=sum(payment_methodpromo_code_match)
  
)

donations.by.person.agg <- subset(donations.by.person.agg, mean_donation_optional_support < 100000)
donations.by.person.agg <- subset(donations.by.person.agg, nb_donation < 10000)
donations.by.person.agg <- subset(donations.by.person.agg, nb_payment_methodpromo_code_match < 10000)

rm(list=c("tmp"))
gc(TRUE)

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
