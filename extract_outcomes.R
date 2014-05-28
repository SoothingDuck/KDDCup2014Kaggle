library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

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
