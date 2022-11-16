pizza <- read.table('Frozen_Pizza.txt', sep = '\t', header = TRUE)

#Baltimore
plot(pizza$Baltimore.Price,pizza$Baltimore.Volume,xlab='price',ylab='volume')

#fitted model
imod <- lm(Baltimore.Volume~Baltimore.Price,data=pizza)
summary(imod)
summary(imod)$coefficients
confint(imod, level = 0.95)

#Dallas
plot(pizza$Dallas.Price,pizza$Dallas.Volume,xlab='price',ylab='volume')
#fitted model
imod2 <- lm(Dallas.Volume~Dallas.Price,data=pizza)
summary(imod2)
summary(imod2)$coefficients
confint(imod2, level = 0.95)

#chicago
plot(pizza$Chicago.Price,pizza$Chicago.Volume,xlab='price',ylab='volume')
#fitted model
imod3 <- lm(Chicago.Volume~Chicago.Price,data=pizza) 
summary(imod3) 
summary(imod3)$coefficients 
confint(imod3, level = 0.95)

#Denver
plot(pizza$Denver.Price,pizza$Denver.Volume,xlab='price',ylab='volume')
#fitted model
imod4<- lm(Denver.Volume~Denver.Price,data=pizza)
summary(imod4)
summary(imod4)$coefficients 
confint(imod4, level = 0.95)

#2
#Baltimore
pizza$Week <- as.Date(pizza$Week,format = "%m/%d/%y")
par(mfrow = c(1, 2))
plot(pizza$Week,imod$residuals,xlab = 'Time', ylab = 'Residual') 
abline(a = 0, b = 0) 
plot(imod$fitted.values, imod$residuals, xlab = 'Fitted value', ylab = 'Residual') 
abline(a = 0, b = 0)

#Q-Q plot Baltimore
par(mfrow = c(1, 2)) 
hist(imod$residuals, xlab = 'Residual', main = 'Histogram of Residuals') 
qqnorm(imod$residuals) 
qqline(imod$residuals)

#Dallas
par(mfrow = c(1, 2)) 
plot(pizza$Week,imod2$residuals,xlab = 'Time', ylab = 'Residual') 
abline(a = 0, b = 0) 
plot(imod2$fitted.values, imod2$residuals, xlab = 'Fitted value', ylab = 'Residual') 
abline(a = 0, b = 0)

#QQ plot
par(mfrow = c(1, 2)) 
hist(imod2$residuals, xlab = 'Residual', main = 'Histogram of Residuals') 
qqnorm(imod2$residuals)
qqline(imod2$residuals)

#Chigago
par(mfrow = c(1, 2)) 
plot(pizza$Week,imod3$residuals,xlab = 'Time', ylab = 'Residual') 
abline(a = 0, b = 0) 
plot(imod3$fitted.values, imod3$residuals, xlab = 'Fitted value', ylab = 'Residual') 
abline(a = 0, b = 0)

#QQ plot
par(mfrow = c(1, 2)) 
hist(imod3$residuals, xlab = 'Residual', main = 'Histogram of Residuals') 
qqnorm(imod3$residuals)
qqline(imod3$residuals)

#Denver
par(mfrow = c(1, 2)) 
plot(pizza$Week,imod4$residuals,xlab = 'Time', ylab = 'Residual') 
abline(a = 0, b = 0) > plot(imod4$fitted.values, imod4$residuals, xlab = 'Fitted value', ylab = 'Residual')
abline(a = 0, b = 0)

#QQ Plot
par(mfrow = c(1, 2)) 
hist(imod4$residuals, xlab = 'Residual', main = 'Histogram of Residuals') 
qqnorm(imod4$residuals) 
qqline(imod4$residuals)

#3
confint(imod2, level = 0.90)

#4
imod2 <- lm(pizza$Dallas.Volume~pizza$Dallas.Price,data=pizza) 
summary(imod2)

#5
# if the price is $2.5
range(pizza$Dallas.Price) 
new<-data.frame(Dallas.Price=c(2.50)) 
predict(imod2,newdata = new,interval='confidence',level=0.95)

#if the price is $3
new1<-data.frame(Dallas.Price=c(3.00)) 
predict(imod2,newdata = new1,interval='confidence',level=0.95)


#6
dallasnew <-lm(pizza$Dallas.Volume~pizza$Dallas.Price,data=pizza)
new2<-data.frame(Dallas.Price=c(2.99)) 
predict(dallasnew,newdata = new2,interval='prediction',level=0.95) 


