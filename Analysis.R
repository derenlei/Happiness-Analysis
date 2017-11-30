data = read.table("projdata.txt",header = T)
pairs(data)
happy = data$happy
gender = data$gender
workhrs = data$workhrs
relationship = data$relationship
model1 = lm(happy~gender+workhrs+relationship)
summary(model1)
anova(model1)
