View(states.data)
# summary of metro and energy columns, all rows
sts.met.eng <- subset(states.data, select = c("metro", "energy"))
summary(sts.met.eng)
# correlation between metro and energy
cor(sts.met.eng)
# scatter plot of metro  vs energy
plot(sts.met.eng)
# Fit our regression model
eng.mod <- lm(energy ~ metro, data=states.data) 
# Summarize and print the results
summary(eng.mod) 
#plot model
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(eng.mod, which = c(1, 2)) # "which" argument optional

#Let's add a new predictors (green and area)
sts.met.eng.gr.area <- subset(states.data, select = c("metro", "energy", "green","area"))
summary(sts.met.eng.gr.area)
cor(sts.met.eng.gr.area)
plot(sts.met.eng.gr.area)
eng.mod2 <- lm(energy ~ metro + green + area, data=states.data) 
summary(eng.mod2) 
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(eng.mod2, which = c(1, 2)) # "which" argument optional

## This model is significantly better than the model with metro as a only predictor






