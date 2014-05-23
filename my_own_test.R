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

# toute les données
all.data <- merge(set.data, essay.data, on=c("projectid"))
all.data <- merge(all.data, projects.data, on=c("projectid"))

rm(list=c("set.data","essay.data","projects.data"))

# normalization


# séparation train et test
test.data <- subset(all.data, typedataset == "test")
train.data <- subset(all.data, typedataset == "train")

train.data <- merge(train.data, outcomes.data, on=c("projectid"))
