source("functions.R")

projects.train.is.exciting.all <- get.projects.data.train(force=TRUE, variable="is_exciting")

projects.train.is.exciting.all <- get.projects.data.test(force=TRUE)

