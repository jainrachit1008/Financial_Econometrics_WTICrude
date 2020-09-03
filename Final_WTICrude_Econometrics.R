## setwd('/Users/rachnish/Documents/Stevens/FIN 620 Lectures') ##

library(EIAdata)
library(quantmod)
require(tseries)
library(fBasics)
library(Quandl)
library(MTS)
library(urca)
library(astsa)
require(fGarch)
require(rugarch)
require(quantreg)
install.packages('mvtnorm', dep = TRUE)

#DCOILWTICO = Quandl("FRED/DCOILWTICO", start_date="2010-03-01", end_date="2020-02-29")

############################ Part1 ##################################
getSymbols("DCOILWTICO", src = "FRED", from = "2010-03-03",adjust = TRUE)
plot(DCOILWTICO, main = "Figure 1: DCOILWTICO Daily Prices", type = "l")
dim(DCOILWTICO)
 
# Calculating Daily, Log Daily, Weekly and Monthly Return
wtird=periodReturn(DCOILWTICO,period="daily")
wtirl=periodReturn(DCOILWTICO,period="daily",type="log")
wtirw=periodReturn(DCOILWTICO,period="weekly")
wtirm=periodReturn(DCOILWTICO,period="monthly")
wtira=periodReturn(DCOILWTICO,period="annually")

# Basic Statistics of log daily returns
wtirl_mean <- mean(wtirl)
wtirl_sd <- sd(wtirl)
wtirl_skew <- skewness(wtirl)
wtirl_Kurtosis <- kurtosis(wtirl)

plot(wtirl,main="WTI Crude daily log returns")


# Checking the normality of the Log Returns
qqnorm(wtirl,main="Q-Q plot of the log-returns")
qqnorm(wtirw,main="Q-Q plot of the weekly-returns")
qqnorm(wtirm,main="Q-Q plot of the Monthly-returns")
qqnorm(wtira,main="Q-Q plot of the Annually-returns")

#Expressing Daily, Weekly and Monthly Prices
chartSeries(DCOILWTICO)
Weekly_Prices <- to.weekly(DCOILWTICO,drop.time = TRUE)
par(mfrow=c(1, 1))
plot(Weekly_Prices)
Monthly_Prices <- to.monthly(DCOILWTICO, drop.time = TRUE)
plot(Monthly_Prices)
Annual_Prices <- to.yearly(DCOILWTICO, drop.time = TRUE)
plot(Annual_Prices)

#t-test of all returns
t.test(wtird)
t.test(wtirl)
t.test(wtirw)
t.test(wtirm)
t.test(wtira)

#Histogram of returns
hist(wtird, nclass = 40, main = "Daily")
hist(wtirl, nclass = 40, main = "Log-Daily")
hist(wtirw, nclass = 40, main = "Weekly")
hist(wtirm, nclass = 40, main = "Monthly")
hist(wtira, nclass = 40, main = "Yearly")


#Box Test
Box.test(wtird)
Box.test(wtirl)
Box.test(wtirw)
Box.test(wtirm)
Box.test(wtira)

#acf
l = acf(wtird[2:dim(wtird)[1]])
m = acf(wtirl[2:dim(wtirl)[1]])
n = acf(wtirw[2:dim(wtirw)[1]])
o = acf(wtirm[2:dim(wtirm)[1]])
p = acf(wtira[2:dim(wtira)[1]])

#pacf
pacf(wtirm[2:dim(wtirm[1])])
pacf(wtira[2:dim(wtirm[1])])

source("lagplot.R")

lagplot(wtird)

############################ Part2 ##################################

#Monthly log returns of WTI Spot
wtirml=periodReturn(DCOILWTICO,period="monthly", type = "log")
mon_logret = as.numeric(wtirml[1:410,])
plot(mon_logret,main="WTI Crude monthly log returns", type = "l")

#Monthly log returns of WTI Futures
wti = getEIA("PET.RCLC1.M", key = '678a30093a51e4ce3a41880cec5213d9')
wti = wti[34:443,]
futures_logret = as.numeric(periodReturn(wti,period="monthly", type = "log"))
plot(futures_logret,main="WTI Crude futures monthly log returns", type = "l")
basicStats(futures_logret)
t.test(futures_logret)
hist(futures_logret,nclass=40)
d=density(futures_logret)
names(d)
plot(d$x,d$y,type='l', main = "Futures Monthly Log Returns (Density Plot)")
mu=mean(futures_logret) 
s1 = sd(futures_logret)
#create a sequence of real numbers from -0.4 to 0.4 with increment 0.01.
x=seq(-0.4,0.4,0.01) 
#obtain normal density with mean mu and standard deviation s1.
y=dnorm(x,mean=mu,sd=s1) 
#impose a dashed line on the density plot for comparison with normal density.
lines(x,y,lty=2) 
#you can also use different colors in the plot. For example,
lines(x,y,col="red") 

#Checking Seasonality of WTI Spot prices & Futures Log Returns
k1 = ts(mon_logret, frequency = 12)
v1 <- decompose(k1)
plot(v1)
k2 = ts(futures_logret, frequency = 12)
v2 <- decompose(k2)
plot(v2)

#Checking the normality of the Log Monthly Returns of Spot & Futures Log Returns
qqnorm(mon_logret,main="Q-Q plot of the Spot Monthly Log-returns")
qqnorm(futures_logret,main="Q-Q plot of the Futures Monthly Log-returns")

#Basic Statistics of log monthly returns
wtirml_mean <- mean(mon_logret)
print(wtirml_mean)     #coming to 0.00007746481
wtirml_sd <- sd(mon_logret)
print(wtirml_sd)        #coming to 0.02494914
wtirml_skew <- skewness(mon_logret)
print(wtirml_skew)       #coming to -0.6286567
wtirml_Kurtosis <- kurtosis(mon_logret)
print(wtirml_Kurtosis)       #coming to 13.52238
basicStats(mon_logret)


#JB Test on Monthly Log Returns of Spot & Futures prices(Normality test)
jarque.bera.test(mon_logret)  # p-value is less than 0.05 hence not-normal
jarque.bera.test(futures_logret)  # p-value is less than 0.05 hence not-normal

#ACF & PACF of Monthly log returns of Spot & Futures Log Returns
x =acf(mon_logret)
x
pacf(mon_logret)
y = acf(futures_logret)
y
pacf(futures_logret)
#we see from the acf graph that the monthly returns have a seasonality component. Hence, not feasible to use AR or MA Model

#Lagplots
lagplot(mon_logret)
lagplot(mon_logret,2)
lagplot(mon_logret,3)
lagplot(mon_logret, lag = 12)
lagplot(mon_logret, lag = 11)
lagplot(mon_logret, lag = 10)

#Ljung-Box Q(m) Box test
Box.test(mon_logret, type = "Ljung")  # there is serial correlation, p value < 0.05
Box.test(futures_logret, type = "Ljung") # there is serial correlation, p value < 0.05

#Fitting AR Model
m1 = ar(mon_logret,method = "mle")
names(m1)
m1$order
m11 = ar(futures_logret, method = "mle")
names(m11)
m11$order

#ARIMA Model with order = 1 for Spot & Futures Log returns
m2 <- arima(mon_logret,order = c(1,0,0))
m2
names(m2)
tsdiag(m2)
m22 <- arima(futures_logret,order = c(1,0,0))
m22
names(m22)
tsdiag(m22)
p1 <-c(1,-m2$coef[1:3]) 
p1
p11 <-c(1,-m22$coef[1:3]) 
p11
predict(m2,1)   # predicted value = -0.02161056
predict(m2,2)    # predicted value = -0.002506582
predict(m22,1)  # predicted value = -0.03408707
predict(m22,2)  # predicted value = -0.007861376

#Auto - ARIMA for seasonal time series
require(forecast)
auto.arima(mon_logret)
auto.arima(futures_logret)

#ARIMA Model with order = (4,0,3) for spot log returns
m3<- arima(mon_logret, order = c(4,0,3))
m3
p2 <-c(1,-m3$coef[1:3]) 
p2
predict(m3,1)   # predicted value = -0.008995914
predict(m3,2)    # predicted value = 0.019516389
predict(m3,11)     # predicted value = 0.003341314
tsdiag(m3)

#ARIMA Model with order = (5,0,4) for futures log returns
m33<- arima(futures_logret, order = c(5,0,4))
m33
p22 <-c(1,-m33$coef[1:4]) 
p22
predict(m33,1)   # predicted value = -0.04191437
predict(m33,2)    # predicted value = 0.01111583
predict(m33,11)     # predicted value = 0.001114679
tsdiag(m33)

#Auto - ARIMA for price seasonal time series
Mon_prices <- as.numeric(Monthly_Prices)
auto.arima(Mon_prices)
m4<- arima(Mon_prices, order = c(1,1,3))
m4
predict(m4,1)
predict(m4,2)
tsdiag(m4)

#Augmented Dickey-Fuller test for Spot & Futures
adf.test(mon_logret)
adf.test(futures_logret)
z <- diffM(mon_logret)
k <- diffM(futures_logret)
adf.test(z)
adf.test(k)

############################ Part3 ##################################
# Create a dataframe with spot & futures log returns (A Bivariate Time-Series)
spot_logret = as.data.frame(wtirml[1:410,])
fut_logret = as.data.frame(periodReturn(wti,period="monthly", type = "log"))
xt = cbind(spot_logret,fut_logret)
names(xt) <- c("Spot_monthly_logret","Futures_monthly_logret")
basicStats(xt)
head(xt)

# Concurrent Scatterplots for Spot vs. futures returns
par(mfrow=c(1,1))
plot(mon_logret, futures_logret, title(main = "Concurrent Correlation Scatterplot"))

# Lag Scatterplots for Spot vs. futures returns
cor(spot_logret,fut_logret)


# Bi-variate analysis
MTSplot(xt)
ccm(xt)
rt=diffM(xt)
ccm(rt)
  # Multivariate Portmanteau Tests for xt & its first differencing
  mq(xt,lag=10)
  mq(rt,lag=10)


# Implementing VAR Model
VARorder(xt)    # selected order 4 based on bic criteria
X1=VAR(xt,4)
MTSdiag(X1)   # Model Validation
VARpred(X1,4)   # predition, 1-step to 4-step forecasts.

# Co-Integration Tests
X2 = lm(mon_logret~futures_logret)
X2
wt=mon_logret-0.8267*futures_logret
acf(wt)
pacf(wt)
plot(wt,type='l')
abline(h=c(0))
abline(h=c(0.1),col="red")  
abline(h=c(-0.1),col="blue")
X3 = arima(wt,order =c(1,0,0))
X3
tsdiag(X3)
plot(wt,type = 'l')
O1 <- ar(xt)
O1$order
X4=ca.jo(xt,K=4)
summary(X4)
wt2=mon_logret-0.9811*futures_logret
acf(wt2)
pacf(wt2)
X5 = arima(wt2,order = c(1,0,0))
X5
tsdiag(X5)
plot(wt2, type = 'l')

# GARCH Models
at = mon_logret - mean(mon_logret)
Box.test(at^2,lag=10,type="Ljung")  # ARCH Test
G1 = garchFit(~garch(1,1),data = mon_logret)
G1 = garchFit(~garch(1,1),data = mon_logret,trace = F)
summary(G1)    # Obtain results and model checking statistics 
sresi=residuals(G1,standardize=T)    # Obtain standardized residuals -epsilon(t)-hat 
sigma.t = volatility(G1)    # obtain the fitted volatility sigma_t
plot(G1)
predict(G1,6)    #  predictions of 1-step to 6-step ahead.
# GARCH Models with student-t distribution
G2 = garchFit(~garch(1,1),data = mon_logret,trace = F,cond.dist = "std")
summary(G2)
sresi=residuals(G2,standardize=T)    # Obtain standardized residuals -epsilon(t)-hat 
sigma.t = volatility(G2)    # obtain the fitted volatility sigma_t
plot(G2)
# GARCH Models with skewed student-t distribution
G3 = garchFit(~garch(1,1),data = mon_logret,trace = F,cond.dist = "sstd")
summary(G3)
# ARMA & GARCH together
G4 = garchFit(~arma(0,1)+garch(1,0),data = mon_logret,trace = F)
summary(G4)

# IGARCH Modelling
source("Igarch.R")
G5 = Igarch(mon_logret)   #if mu and omega are zero
names(G5)
G6 = Igarch(mon_logret,include.mean = T)  #if mu is not zero
G6a = Igarch(mon_logret,volcnt = T)   #also estimate omega

# GARCH-M Model
source("garchM.R")
G7 = garchM(mon_logret)
names(G7)

# TGARCH & E-GARCH Model
source("Tgarch11.R")
G8 = Tgarch11(mon_logret)
names(G8)
resi=G8$residuals/G8$volatility   # Model checking
Box.test(resi,lag=12,type="Ljung")
Box.test(resi^2,lag=12,type="Ljung") 
source("Egarch.R")
G9 = Egarch(mon_logret)
names(G9)
plot(G9$volatility,type = "l")

# Specification of Standard GARCH(1,1)/IGARCH/eGARCH model etc using rugarch
spec1=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0))) 
# and now, estimation
mm = ugarchfit(spec=spec1,data=mon_logret)
mm
plot(mm)
# prediction, 1 step to 4 step ahead
p3 = ugarchforecast(mm,n.ahead = 4)
p3
sigma(p3)
fitted(p3)

############################ Part4 ##################################
nwtirml = -mon_logret

### RiskMetrics #########
source("RMfit.R")
RMfit(nwtirml)
RMfit(nwtirml,estim=F)

G10 = garchFit(~garch(1,1),data = nwtirml,trace = F)
summary(G10)
pm1=predict(G10,10)
pm1
## IGARCH and GARCH gave us very close predictions of vol -> 0.1105 and 0.1201

source("RMeasure.R")
RMeasure(-.0032,.1201)
names(pm1)

#Another GARCH with student-t dist
G11=garchFit(~garch(1,1),data=nwtirml,trace=F,cond.dist="std")  # t-dist
summary(G11)
pm2=predict(G11,1)
pm2
RMeasure(-0.003939,0.1197,cond.dist="std",df=10)

#Quantile Measurement Technique
# quantile(nwtirml,c(0.95,0.99,0.999))
# G12=rq(nwtirml~vol+vix,data=wtirml,tau=0.95)
# summary(G12)

### Extreme value theory
require(evir)
G13 = gev(nwtirml,block=21)
G13
source("evtVaR.R")
evtVaR(0.2488,0.0108,0.0129)

### Peaks over threshold
G14=pot(nwtirml,thres=0.01)
plot(G14)



