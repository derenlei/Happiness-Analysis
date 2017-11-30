data = read.table("D:/PSTAT126/Happiness-Analysis/projdata.txt",header = T)

# Inspect Scatterplot Matrix
pairs(data)

# Fit 1st order model
happy = data$happy
gender = data$gender
workhrs = data$workhrs
relationship = data$relationship
model1 = lm(happy~gender+workhrs+relationship)
# Test overall model and partial sum of square
summary(model1)
# Test sequential sum of square
anova(model1)

# Fit 2nd order model
full=lm(happy~.^2,data=data)
summary(full)
# Extra SS for interaction
anova(model1,full)
# Interaction plot
plot(hours,exam,xlim=c(0,20),ylim=c(10,75),xlab="Hours of Study",ylab="Exam Score",pch=19,col="blue")
points(hours[major==1],exam[major==1],pch=19,col="red")
abline(lm(exam[major==0]~hours[major==0]),col="blue")
abline(lm(exam[major==1]~hours[major==1]),col="red")
legend("topleft",inset=.05,cex=.75,pch=19,lty=c(1,1),col=c("red","blue"),legend=c("Psychology Majors","Mathematics Majors"))