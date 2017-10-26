#Add the interaction to the model in exercise 1
eng.mode.by.population <- lm(energy ~ metro*pop, data=states.data) 
#show results
coef(summary(eng.mode.by.population))
# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
eng.region <- lm(energy ~ region, data=states.data) 
#Show the results
coef(summary(eng.region)) # show regression coefficients table
anova(eng.region) # show ANOVA table
# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(energy ~ C(region, base=4),data=states.data)))
# change the coding scheme
coef(summary(lm(energy ~ C(region, contr.helmert),data=states.data)))

## Yes there are significant differences between the 4 regions.