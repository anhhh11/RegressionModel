library(UsingR)
data(diamond)
y <- diamond$price
x <- diamond$carat
fit <- lm(y~x)
sumCoef <- summary(fit)$coefficient
#beta0
sumCoef[1,1] + c(-1,1)*qt(.975,df=fit$df) * sumCoef[1,2]
#beta1
sumCoef[2,1] + c(-1,1)*qt(.975,df=fit$df) * sumCoef[2,2]
#With 95% confidence, estimate that 1 carat increase result in 3566 to 3885 increase in price

#predict
x = seq(1,15,by=.5)
y = 2*x + 5
x[30]=2
y[30]=20
x[31]=13
y[31]=50
xVals = seq(min(x),max(x),by=.5)
newdata <- data.frame(x=xVals)
p1 <- predict(fit,newdata,interval=("confidence"))
p2 <- predict(fit,newdata,interval=("prediction"))
pgtlot(x,y)
abline(fit,lwd=2)
lines(xVals,p1[,2]); lines(xVals,p1[,3])
lines(xVals,p2[,2]); lines(xVals,p2[,3])
