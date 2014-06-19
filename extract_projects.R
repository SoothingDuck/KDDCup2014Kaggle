library(RSQLite)
library(plyr)
library(reshape2)

sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

print("Extraction donnees projets...")

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
T1.fulfillment_labor_materials as fulfillment_labor_materials,
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

# reduction
library(lubridate)
projects.data$date_posted <- ymd(projects.data$date_posted)
projects.data$days_since_posted <- (as.integer(ymd("2014-05-12") - projects.data$date_posted)/(3600*24))
# projects.data <- subset(projects.data, days_since_posted <= 1500)
projects.data <- subset(projects.data, days_since_posted <= 350)

# primary_subject:secondary_subject
t <- data.frame(model.matrix(~ primary_focus_subject:secondary_focus_subject, data=projects.data))
t <- t[,2:ncol(t)]
t$projectid <- projects.data$projectid
m <- melt(t, id.vars="projectid")
u <- subset(m, value > 0)
s <- data.frame(table(u$variable))
s <- s[order(-s$Freq),]
s.list <- s$Var1[1:50]
m <- subset(m, variable %in% s.list)
v <- dcast(m, projectid ~ variable)
names(v) <- c("projectid", paste("primary_focus_merge", names(v[2:ncol(v)]), sep="."))
projects.data <- merge(projects.data, v, by="projectid")
# Fin primary_subject:secondary_subject

# primary_focus_area:primary_focus_subject
t <- data.frame(model.matrix(~ primary_focus_area:primary_focus_subject, data=projects.data))
t <- t[,2:ncol(t)]
t$projectid <- projects.data$projectid
m <- melt(t, id.vars="projectid")
u <- subset(m, value > 0)
s <- data.frame(table(u$variable))
s <- s[order(-s$Freq),]
s.list <- s$Var1[1:50]
m <- subset(m, variable %in% s.list)
v <- dcast(m, projectid ~ variable)
names(v) <- c("projectid", paste("primary_focus_merge", names(v[2:ncol(v)]), sep="."))
projects.data <- merge(projects.data, v, by="projectid")
# Fin primary_focus_area:primary_focus_subject

# primary_area:secondary_area
t <- data.frame(model.matrix(~ primary_focus_area:secondary_focus_area, data=projects.data))
t <- t[,2:ncol(t)]
t$projectid <- projects.data$projectid
m <- melt(t, id.vars="projectid")
u <- subset(m, value > 0)
s <- data.frame(table(u$variable))
s <- s[order(-s$Freq),]
s.list <- s$Var1[1:50]
m <- subset(m, variable %in% s.list)
v <- dcast(m, projectid ~ variable)
names(v) <- c("projectid", paste("primary_focus_merge", names(v[2:ncol(v)]), sep="."))
projects.data <- merge(projects.data, v, by="projectid")
# Fin primary_area:secondary_area

# normalization
projects.data$typedataset <- factor(projects.data$typedataset)
projects.data$school_ncesid_status <- factor(ifelse(is.na(projects.data$school_ncesid), "NotAvailable", "Available"))
# projects.data$school_city_big <- factor(ifelse(
#   projects.data$school_city %in% names(which(prop.table(table(projects.data$school_city)) > 0.01)),
#   projects.data$school_city,
#   "SmallCity"
# ))

# school_state
projects.data$school_state <- factor(toupper(projects.data$school_state))
t <- model.matrix(~ school_state, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("school_state", colnames(t))])
# Fin school_state

# school_district
projects.data$school_district <- factor(toupper(projects.data$school_district))
u <- data.frame(table(projects.data$school_district))
u <- u[order(-u$Freq),]
projects.data$school_district_restriction <- factor(ifelse(as.character(projects.data$school_district) %in% as.character(u$Var1[1:100]), as.character(projects.data$school_district), "OTHER"))
t <- model.matrix(~ school_district_restriction, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("school_district_restriction", colnames(t))])
projects.data <- projects.data[, colnames(projects.data) != "school_district_restriction"]
# Fin school_district

# school_county
projects.data$school_county <- factor(toupper(projects.data$school_county))
u <- data.frame(table(projects.data$school_county))
u <- u[order(-u$Freq),]
projects.data$school_county_restriction <- factor(ifelse(as.character(projects.data$school_county) %in% as.character(u$Var1[1:100]), as.character(projects.data$school_county), "OTHER"))
t <- model.matrix(~ school_county_restriction, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("school_county_restriction", colnames(t))])
projects.data <- projects.data[, colnames(projects.data) != "school_county_restriction"]
# Fin school_county

projects.data$school_metro <- factor(ifelse(projects.data$school_metro == "", "Unknown", projects.data$school_metro))

projects.data$school_charter <- factor(ifelse(projects.data$school_charter == "t", "Yes", "No"))
projects.data$school_magnet <- factor(ifelse(projects.data$school_magnet == "t", "Yes", "No"))
projects.data$school_year_round <- factor(ifelse(projects.data$school_year_round == "t", "Yes", "No"))
projects.data$school_nlns <- factor(ifelse(projects.data$school_nlns == "t", "Yes", "No"))
projects.data$school_kipp <- factor(ifelse(projects.data$school_kipp == "t", "Yes", "No"))
projects.data$school_charter_ready_promise <- factor(ifelse(projects.data$school_charter_ready_promise == "t", "Yes", "No"))

projects.data$teacher_prefix[projects.data$teacher_prefix == ""] <- "Mr." 
projects.data$teacher_prefix[projects.data$teacher_prefix == "Dr."] <- "Mr." 
projects.data$teacher_prefix[projects.data$teacher_prefix == "Mr. & Mrs."] <- "Mr." 
projects.data$teacher_prefix <- factor(projects.data$teacher_prefix)

projects.data$teacher_teach_for_america <- factor(ifelse(projects.data$teacher_teach_for_america == "t", "Yes", "No"))
projects.data$teacher_ny_teaching_fellow <- factor(ifelse(projects.data$teacher_ny_teaching_fellow == "t", "Yes", "No"))

# projects.data$primary_focus_subject[projects.data$primary_focus_subject == ""] <- "Literacy"
projects.data$primary_focus_subject <- factor(projects.data$primary_focus_subject)

# projects.data$primary_focus_area[projects.data$primary_focus_area == ""] <- "Literacy & Language"
projects.data$primary_focus_area <- factor(projects.data$primary_focus_area)

projects.data$secondary_focus_subject <- factor(projects.data$secondary_focus_subject)
projects.data$secondary_focus_area <- factor(projects.data$secondary_focus_area)

# projects.data$resource_type[projects.data$resource_type == ""] <- "Supplies"
projects.data$resource_type <- factor(projects.data$resource_type)

projects.data$poverty_level <- factor(projects.data$poverty_level)

# projects.data$grade_level[projects.data$grade_level == ""] <- "Grades PreK-2"
projects.data$grade_level <- factor(projects.data$grade_level)

projects.data$students_reached <- ifelse(is.na(projects.data$students_reached), 30.0, projects.data$students_reached)

projects.data$eligible_double_your_impact_match <- factor(ifelse(projects.data$eligible_double_your_impact_match == "t", "Yes", "No"))
projects.data$eligible_almost_home_match <- factor(ifelse(projects.data$eligible_almost_home_match == "t", "Yes", "No"))

projects.data$month_posted <- factor(month(projects.data$date_posted))
projects.data$year_posted <- factor(year(projects.data$date_posted), ordered=TRUE)

projects.data$day_of_week_posted <- factor(weekdays(projects.data$date_posted))

projects.data$fulfillment_labor_materials <- factor(projects.data$fulfillment_labor_materials)


agg <- ddply(projects.data,
             .(schoolid),
             summarise,
             nb.projects.for.school=length(schoolid))

projects.data <- merge(projects.data, agg, on=c("schoolid"))

agg <- ddply(projects.data,
             .(teacher_acctid),
             summarise,
             nb.projects.for.teacher=length(teacher_acctid))

projects.data <- merge(projects.data, agg, on=c("teacher_acctid"))

agg <- ddply(subset(projects.data, ! is.na(school_ncesid)),
             .(school_ncesid),
             summarise,
             nb.distinct.school.by.ncesid=length(unique(schoolid))
)

projects.data <- merge(projects.data, agg, on=c("school_ncesid"), all.x = TRUE)
projects.data$nb.distinct.school.by.ncesid <- with(projects.data, factor(ifelse(is.na(nb.distinct.school.by.ncesid), 1, nb.distinct.school.by.ncesid)))

agg <- ddply(projects.data,
             .(school_state),
             summarise,
             nb.projects.by.state=length(school_state)
)

projects.data <- merge(projects.data, agg, on=c("school_state"))
projects.data <- projects.data[, colnames(projects.data) != "school_state"]



agg <- ddply(projects.data,
             .(school_city),
             summarise,
             nb.projects.by.city=length(school_city)
)

projects.data <- merge(projects.data, agg, on=c("school_city"))

projects.data <- subset(projects.data, ! is.na(school_zip))
agg <- ddply(projects.data,
             .(school_zip),
             summarise,
             nb.projects.by.zip=length(school_zip)
)

projects.data <- merge(projects.data, agg, on=c("school_zip"))

agg <- ddply(projects.data,
             .(school_district),
             summarise,
             nb.projects.by.district=length(school_district)
)

projects.data <- merge(projects.data, agg, on=c("school_district"))

agg <- ddply(projects.data,
             .(school_county),
             summarise,
             nb.projects.by.county=length(school_county)
)

projects.data <- merge(projects.data, agg, on=c("school_county"))

# primary_focus_subject
t <- model.matrix(~ primary_focus_subject, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("primary_focus_subject", colnames(t))])
projects.data <- projects.data[, colnames(projects.data) != "primary_focus_subject"]
# fin primary_focus_subject

# secondary_focus_subject
t <- model.matrix(~ secondary_focus_subject, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("secondary_focus_subject", colnames(t))])
projects.data <- projects.data[, colnames(projects.data) != "secondary_focus_subject"]
# fin secondary_focus_subject

# primary_focus_area
t <- model.matrix(~ primary_focus_area, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("primary_focus_area", colnames(t))])
projects.data <- projects.data[, colnames(projects.data) != "primary_focus_area"]
# fin primary_focus_subject

# secondary_focus_area
t <- model.matrix(~ secondary_focus_area, data=projects.data)
projects.data <- cbind(projects.data, t[,grepl("secondary_focus_area", colnames(t))])
projects.data <- projects.data[, colnames(projects.data) != "secondary_focus_area"]
# fin secondary_focus_subject

# primary_focus_subject:secondary_focus_subject
# t <- model.matrix(~ primary_focus_subject:secondary_focus_subject, data=projects.data)
# projects.data <- cbind(projects.data, t[,grepl("primary_focus_subject", colnames(t))])
# fin primary_focus_subject:secondary_focus_subject

# diff price
projects.data$total_price_optional_support <- with(projects.data, total_price_including_optional_support-total_price_excluding_optional_support)

# Nettoyage
rm(list=c("con", "drv", "sqlitedb.filename", "agg", "t", "u", "m", "s", "v", "s.list"))
gc(TRUE)
