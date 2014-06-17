source("functions.R")
source("extract_essays.R")

projects.data <- get.projects.data(force=FALSE)
# projects.data <- subset(projects.data, typedataset == "train")
projects.data <- subset(projects.data, days_since_posted <= 350)

projects.data <- merge(projects.data, essays.data, by="projectid")

library(tm)
library(plyr)

print("selection")
tmp <- projects.data[, names(projects.data) %in% c("projectid", "title", "short_description", "need_statement.y", "essay")]
names(tmp) <- c("projectid", "title", "short_description", "need_statement", "essay")

print("cleanup")
rm(list=c("essays.data", "projects.data"))
gc(TRUE)

print("vectorize")
docs <- tmp$title
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
                          control=list(
                            weighting=weightTfIdf,
                            stopwords=TRUE))

sparsed.dtm <- removeSparseTerms(dtm, 0.97)

sparsed.dtm.tmp <- inspect(sparsed.dtm)
sparsed.dtm.tmp <- data.frame(sparsed.dtm.tmp)
colnames(sparsed.dtm.tmp) <- paste("word", "title", colnames(sparsed.dtm.tmp), sep=".")

# for(col in colnames(sparsed.dtm.tmp)) {
#   sparsed.dtm.tmp[, col] <- ifelse(sparsed.dtm.tmp[,col] > 0, 1, 0)
# }

sparsed.dtm.tmp$projectid <- tmp$projectid

semantic.title.data <- sparsed.dtm.tmp

save(semantic.title.data, file=file.path("tmp","semantic_title.RData"))
