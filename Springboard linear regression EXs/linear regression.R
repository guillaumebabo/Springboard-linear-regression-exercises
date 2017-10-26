library(ggmap)
library(devtools)
library(tidyr)
library(readr)
library(ggplot2)
getwd()
setwd("~/Desktop/dataSets")
list.files("dataSets")
#read the states data
states.data <- readRDS("states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)
# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)
# scatter plot of expense vs csat
plot(sts.ex.sat)
# Fit our regression model
sat.mod <- lm(csat ~ expense, data=states.data) 
# Summarize and print the results
summary(sat.mod) 
#adding a new variable percent(percentage of student taking the SAT)
summary(lm(csat ~ expense + percent, data = states.data))
#Examine the model object
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]
#Use function methods to get more information about the fit
confint(sat.mod)
#plot model
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional
# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))
#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,data=states.data) 
#Show the results
coef(summary(sat.expense.by.percent)) # show regression coefficients table
# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table
# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),data=states.data)))


