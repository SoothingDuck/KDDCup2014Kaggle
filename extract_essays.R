library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

# Essays data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
essays.data <- dbGetQuery(
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
essays.data <- essays.data[, colnames(essays.data) != "row_names"]
