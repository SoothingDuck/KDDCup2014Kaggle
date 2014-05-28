library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

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

