library(RSQLite)
sqlitedb.filename <- file.path("db", "kdd_cup_data.sqlite3")

# Ressources data
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname=sqlitedb.filename)
resources.data <- dbGetQuery(
  con,
  "
  select
  resourceid as resourceid,
  projectid as projectid,
  vendorid as vendorid,
  vendor_name as vendor_name,
  project_resource_type,
  item_name,
  item_number,
  case when item_unit_price is null then 0 else item_unit_price end as item_unit_price,
  case when item_quantity is null then 0 else item_quantity end as item_quantity
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
  sum(coalesce(item_unit_price,0)*coalesce(item_quantity,0)) as total_price
  from resources
  group by 1,2
  "
)                                         
dbDisconnect(con)
resources.agg.data <- resources.agg.data[, colnames(resources.agg.data) != "row_names"]

library(reshape2)
m <- melt(resources.agg.data, id.vars=c("projectid","project_resource_type"))
m <- subset(m, project_resource_type != "")

resources.by.type <- dcast(m, projectid ~ project_resource_type, sum, fill=0)
