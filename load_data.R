library(RSQLite)

sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")
unlink(sqlitedb.filename)

print("Chargement données Essays")
essays.data <- read.csv("DATA/essays.csv", header=TRUE, sep = ",", stringsAsFactors=FALSE)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)

print("Alimentation table essays")
dbWriteTable(con, "essays", essays.data)

print("Creation des indexes...")
dbGetQuery(con, "create unique index ix_essays_projectid on essays ( projectid )")
dbGetQuery(con, "create index ix_essays_teacher_acctid on essays ( teacher_acctid )")

dbDisconnect(con)
rm(list=c("essays.data"))
gc(TRUE)

print("Chargement données Outcomes")
outcomes.data <- read.csv("DATA/outcomes.csv", header=TRUE, sep = ",", stringsAsFactors=FALSE)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)

print("Alimentation table outcomes")
dbWriteTable(con, "outcomes", outcomes.data)

print("Creation des indexes...")
dbGetQuery(con, "create unique index ix_outcomes_projectid on outcomes ( projectid )")

dbDisconnect(con)
rm(list=c("outcomes.data"))
gc(TRUE)

print("Chargement données Projects")
projects.data <- read.csv("DATA/projects.csv", header=TRUE, sep = ",", stringsAsFactors=FALSE)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)

print("Alimentation table projects")
dbWriteTable(con, "projects", projects.data)

print("Creation des indexes...")
dbGetQuery(con, "create unique index ix_projects_projectid on projects ( projectid )")
dbGetQuery(con, "create index ix_projects_teacher_acctid on projects ( teacher_acctid )")
dbGetQuery(con, "create index ix_projects_schoolid on projects ( schoolid )")

dbDisconnect(con)
rm(list=c("projects.data"))
gc(TRUE)


print("Chargement données Donations")
donations.data <- read.csv("DATA/donations.csv", header=TRUE, sep = ",", stringsAsFactors=FALSE)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)

print("Alimentation table donations")
dbWriteTable(con, "donations", donations.data)

print("Creation des indexes...")
dbGetQuery(con, "create unique index ix_donations_donationid on donations ( donationid )")
dbGetQuery(con, "create index ix_donations_projectid on donations ( projectid )")
dbGetQuery(con, "create index ix_donations_donor_acctid on donations ( donor_acctid )")

dbDisconnect(con)
rm(list=c("donations.data"))
gc(TRUE)


print("Alimentation test set projects")
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)

data <- dbGetQuery(con,
"
select
projectid
from
projects
group by 1
"
  )

data$typedataset <- "train"
sample.submission <- read.csv("DATA/sampleSubmission.csv", header=TRUE, sep = ",", stringsAsFactors=FALSE)

data$typedataset <- ifelse(data$projectid %in% sample.submission$projectid, "test", "train")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)

print("Alimentation table project_dataset")
dbWriteTable(con, "project_dataset", data)

print("Creation des indexes...")
dbGetQuery(con, "create unique index ix_project_dataset_projectid on project_dataset ( projectid )")

dbDisconnect(con)
rm(list=c("data","sample.submission"))
gc(TRUE)
