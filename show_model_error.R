source("functions.R")
library(gbm)

model.filename <- file.path("tmp","model_is_exciting.RData")
load(model.filename)
model.filename <- file.path("tmp","model_fully_funded.RData")
load(model.filename)

projects.train.is.exciting.all <- get.projects.data.train(force=FALSE)
projects.train.is.exciting.all <- subset(projects.train.is.exciting.all, days_since_posted <= 1350)

data <- split.train.test(projects.train.is.exciting.all, .5)

data$train$prediction_is_exciting <- predict(model.is.exciting, newdata=data$train[, model.is.exciting$var.names], n.trees=100, type="response")
data$train$prediction_fully_funded <- predict(model.fully_funded, newdata=data$train[, model.fully_funded$var.names], n.trees=100, type="response")

# affichage erreur
library(ggplot2)

ggplot(data$train) + geom_point(aes(x=prediction_fully_funded,y=prediction_is_exciting,color=is_exciting)) + facet_wrap( ~ fully_funded) + xlim(0,1)
