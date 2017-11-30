data = read.table("C:/Users/Deren Lei/Desktop/UCSB/2017 Fall/PSTAT126/Happiness-Analysis/projdata.txt",header = T)

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
# Interaction plot for workhrs and gender
plot(workhrs[gender==1],happy[gender==1],col="red",pch=19,xlab="workhrs",ylab="happy",xlim=c(10,40),ylim=c(0,10))
abline(lm(happy[gender==1]~workhrs[gender==1]),col="red")
points(workhrs[gender==0],happy[gender==0],col="blue",pch=19)
abline(lm(happy[gender==0]~workhrs[gender==0]),col="blue")
legend("bottomright",inset=.05,cex=.75,pch=19,lty=c(1,1),col=c("red","blue"),legend=c("Women","Men"))
# Interaction plot for relationship and gender
plot(relationship[gender==1],happy[gender==1],col="red",pch=19,xlab="relationship",ylab="happy",xlim=c(0,10),ylim=c(0,10))
abline(lm(happy[gender==1]~relationship[gender==1]),col="red")
points(relationship[gender==0],happy[gender==0],col="blue",pch=19)
abline(lm(happy[gender==0]~relationship[gender==0]),col="blue")
legend("bottomright",inset=.05,cex=.75,pch=19,lty=c(1,1),col=c("red","blue"),legend=c("Women","Men"))

# Model Selection
null=lm(happy~1,data=data)
# Forward
step(null,scope=list(lower=null,upper=full),direction='forward')
# Backward
step(full,direction='backward')
# Both
step(null,scope=list(upper=full),direction='both')
final=lm(happy~gender+workhrs+relationship+relationship:gender,data=data)
summary(final)

# check model
plot(fitted(final),residuals(final),xlab="Fitted value",ylab="Residual",main="Residual plot")
abline(h=0)
qqnorm(residuals(final))
qqline(residuals(final))
hist(residuals(final))
