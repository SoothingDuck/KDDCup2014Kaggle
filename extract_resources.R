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

resources.data$total_price <- with(resources.data, item_unit_price*item_quantity)

# Aggregated Ressources data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
resources.agg.data <- dbGetQuery(
  con,
  "
  select
  projectid as projectid,
  project_resource_type as project_resource_type,
  sum(item_unit_price*item_quantity) as total_price
  from resources
  group by 1,2
  "
)                                         
dbDisconnect(con)
resources.agg.data <- resources.agg.data[, colnames(resources.agg.data) != "row_names"]

library(reshape2)
m <- melt(resources.agg.data, id.vars=c("projectid"))
m <- subset(m, project_resource_type != "")

resources.by.type <- dcast(m, projectid ~ project_resource_type, sum, fill=0)
