model.list.projects.outcomes.filename <- file.path("tmp","model_random_forest_projects_outcomes.RData")
model.list.projects.outcomes.essays.filename <- file.path("tmp","model_random_forest_projects_outcomes_essays.RData")

load(model.list.projects.outcomes.filename)
load(model.list.projects.outcomes.essays.filename)

library(ggplot2)
library(reshape2)

result <- data.frame()

for(variable in names(model.list.projects.outcomes.essays)) {
  model <- model.list.projects.outcomes.essays[[variable]]
  imp <- data.frame(model$importance)
  imp$variable.name <- rownames(imp)
  imp$target.name <- variable
  
  m <- melt(imp, id.vars=c("target.name","variable.name"), na.rm=TRUE)
  
  result <- rbind(result,m)
}


ggplot(subset(result, variable == "Yes")) + 
  geom_tile(aes(x=target.name,y=variable.name, fill=value), colour="white") +
  scale_fill_gradient(low = "white",high = "steelblue") +
  theme(legend.position = "none", 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size=8, angle = 90,  colour = "grey50"))

