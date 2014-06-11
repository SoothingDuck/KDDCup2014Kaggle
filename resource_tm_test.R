source("extract_resources.R")

library(tm)
docs <- data.frame(docs=resources.data$item_name, row.names=resources.data), stringsAsFactors=FALSE)
ds <- DataframeSource(docs)
