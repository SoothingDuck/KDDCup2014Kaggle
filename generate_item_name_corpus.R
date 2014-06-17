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
names(docs) <- as.character(tmp$projectid)
ds <- VectorSource(docs)


print("generation corpus")
corpus <- VCorpus(ds)

print("generatition dtm")
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, toupper)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corpus,
                          control=list(stopwords=TRUE))

sparsed.dtm <- removeSparseTerms(dtm, 0.9)

sparsed.dtm.tmp <- inspect(sparsed.dtm)
sparsed.dtm.tmp <- data.frame(sparsed.dtm.tmp)
colnames(sparsed.dtm.tmp) <- paste("word", colnames(sparsed.dtm.tmp), sep=".")

for(col in colnames(sparsed.dtm.tmp)) {
  sparsed.dtm.tmp[, col] <- ifelse(sparsed.dtm.tmp[,col] > 0, 1, 0)
}

sparsed.dtm.tmp$projectid <- tmp$projectid

semantic.data <- sparsed.dtm.tmp

save(semantic.data, file=file.path("tmp","semantic.RData"))
