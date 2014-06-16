source("functions.R")
source("extract_resources.R")

projects.data <- get.projects.data(force=FALSE)
projects.data <- subset(projects.data, typedataset == "train")
projects.data <- subset(projects.data, days_since_posted <= 350)

resources.data <- merge(resources.data, projects.data, by="projectid")

library(tm)
library(plyr)

print("plyr")
tmp <- ddply(
  resources.data,
  .(projectid),
  summarise,
  item_list=paste(item_name, collapse=" ")
  )


docs <- tmp$item_list
names(docs) <- tmp$projectid
ds <- VectorSource(docs)

print("generation corpus")
corpus <- VCorpus(ds)

print("ecriture corpus")
save(corpus, file=file.path("tmp","corpus.RData"))
