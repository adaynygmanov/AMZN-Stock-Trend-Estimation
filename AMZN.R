## Setting working directory and importing data
setwd("/Users/adaynygmanov/Desktop/Trend\ estimation")
AMZN<- read.csv("/Users/adaynygmanov/Desktop/Trend\ estimation/AMZN.csv")
attach (AMZN)
View(AMZN)
names(AMZN)

## Time Series Plot
#price<-as.vector(t(AMZN[,-c(1,14)]))
price<-ts(Price,start = 2015,frequency = 12)
ts.plot(price,ylab="Price")

## Create equally spaced time points for fitting trends
library(Matrix)
time.pts<-c(1:length(price))
time.pts<-c(time.pts-min(time.pts))/max(time.pts)

## Fit a parametric quadratic polynomial
x1<-time.pts
x2<-time.pts^2
lm.fit<-lm(price~x1+x2)
summary(lm.fit)

## Check for a trend
price.fit.lm<-ts(fitted(lm.fit),start=2015,frequency=12)
ts.plot(price,ylab="Price")
lines(price.fit.lm,lw=2,col="green")
abline(price.fit.lm[1],0,lwd=2,col="blue")

## Local Polynomial Trend Estimation
loc.fit<-loess(price~time.pts)
price.fit.loc<-ts(fitted(loc.fit),start=2015,frequency = 12)

## Splines Trend Estimation
library(mgcv)
gam.fit<-gam(price~s(time.pts))
price.fit.gam<-ts(fitted(gam.fit),start = 2015,frequency = 12)

## Check for a trend
ts.plot(price,ylab="Price")
lines(price.fit.loc,lwd=2,col="brown")
lines(price.fit.gam,lwd=2,col="red")
abline(price.fit.loc[1],0,lwd=2,col="blue")

## Fit a Moving Average
mav.fit<-ksmooth(time.pts, price, kernel = "box")
price.fit.mav<-ts(mav.fit$y,start=2015,frequency = 12)

## Check for a trend
ts.plot(price,ylab="Price")
lines(price.fit.mav,lwd=2,col="purple")
abline(price.fit.mav[1],0,lwd=2,col="blue")

## Compare all estimated trends
all.val<-c(price.fit.mav,price.fit.lm,price.fit.gam,price.fit.loc)
ylim<-c(min(all.val),max(all.val))
ts.plot(price.fit.lm,lwd=2,col="green",ylim=ylim,ylab="Price")
lines(price.fit.mav,lwd=2,col="purple")
lines(price.fit.gam,lwd=2,col="red")
lines(price.fit.loc,lwd=2,col="brown")

legend(x=2015,y=2000,legend=c("MAV","LM","GAM","LOESS"),lty=1, col=c("purple","green","red","brown"))
