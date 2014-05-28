library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

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

