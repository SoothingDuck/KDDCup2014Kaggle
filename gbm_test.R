source("functions.R")

projects.train <- get.projects.data.train(force=TRUE)

cat.cols <- get.categorical.vars()
num.cols <- get.numeric.vars()

all.cols <- union(cat.cols, num.cols)

projects.train.donation <- get.projects.data.train(force=FALSE, variable="donation_from_thoughtful_donor")
