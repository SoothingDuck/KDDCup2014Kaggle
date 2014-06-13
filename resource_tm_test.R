source("extract_resources.R")

library(tm)
library(plyr)

tmp <- ddply(
  resources.data,
  .(projectid),
  summarise,
  item_list=paste(item_name, collapse=" ")
  )

docs <- data.frame(docs=tmp$item_list, row.names=tmp$projectid, stringsAsFactors=FALSE)
ds <- DataframeSource(docs)
