library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

# Ressources data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
resources.data <- dbGetQuery(
  con,
  "
  select
  *
  from resources
  "
)                                         
dbDisconnect(con)
resources.data <- resources.data[, colnames(resources.data) != "row_names"]

