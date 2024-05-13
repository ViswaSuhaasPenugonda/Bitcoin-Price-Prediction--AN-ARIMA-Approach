library(tseries)
library(forecast)
library(ggplot2)
library(zoo)
library(tseries)
library(quantmod)

BTC= read.csv("/Users/suhaaspenugonda/Downloads/bitstampUSD_1-min_data_2012-01-01_to_2021-03-31.csv")
head(BTC)
tail(BTC)
dim(BTC)
summary(BTC)

Open <- as.numeric(BTC$Open)
Close <- as.numeric(BTC$Close)
High <- as.numeric(BTC$High)
Low <- as.numeric(BTC$Low)
Volume <- as.numeric(BTC$Volume)

sum(is.na(Open))
sum(is.na(Close))
sum(is.na(High))
sum(is.na(Low))
sum(is.na(Volume))

par(mfrow=c(3,2))
plot(ts(Open),type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Opening Prices")
plot(ts(Close),type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Closing Prices")
plot(ts(High),type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical High Prices")
plot(ts(Low),type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Low Prices")
plot(ts(Volume),type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Volumes")

sum(is.na(Open))
sum(is.na(Close))
sum(is.na(High))
sum(is.na(Low))
sum(is.na(Volume))

par(mfrow=c(3,2))

ws = 50
Open_s = filter(Open, rep(1/ws, ws), sides = 2)
plot(Open_s,type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Opening Prices")

ws = 50
Close_s = filter(Close, rep(1/ws, ws), sides = 2)
plot(Close_s,type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Closing Prices")

ws = 50
High_s = filter(High, rep(1/ws, ws), sides = 2)
plot(High_s,type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical High Prices")

ws = 50
Low_s = filter(Low, rep(1/ws, ws), sides = 2)
plot(Low_s,type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Low Prices")

ws = 200
Volume_s = filter(Volume, rep(1/ws, ws), sides = 2)
plot(Volume_s,type="l", xlab="Time", ylab = "Price (USD)", main="Bitcoin Historical Volumes")

diff_Open <- diff(Open)
diff_Close <- diff(Open)
diff_High <- diff(High)
diff_Low <- diff(Low)
diff_Volume <- diff(Volume)

par(mfrow=c(3,2))
plot(diff_Open,type="l", xlab="Time", ylab = "Price (USD)", main="Differenced Bitcoin Historical Opening Prices")
plot(diff_Close,type="l", xlab="Time", ylab = "Price (USD)", main="Differenced Bitcoin Historical Closing Prices")
plot(diff_High,type="l", xlab="Time", ylab = "Price (USD)", main="Differenced Bitcoin Historical High Prices")
plot(diff_Low,type="l", xlab="Time", ylab = "Price (USD)", main="Differenced Bitcoin Historical Low Prices")
plot(diff_Volume,type="l", xlab="Time", ylab = "Price (USD)", main="Differenced Bitcoin Historical Volumes")

par(mfrow=c(3,2))
acf1 <- acf(diff_Open, main = "ACF for Residuals of Open")
acf2 <- acf(diff_Close, main = "ACF for Residuals of Close")
acf3 <- acf(diff_High, main = "ACF for Residuals of High")
acf4 <- acf(diff_Low, main = "ACF for Residuals of Low")

ccf_res1 <- ccf(diff_Open, diff_Close, main = "Cross-Correlation of Residuals of Open/Close")
ccf_res2 <- ccf(diff_High, diff_Low, main = "Cross-Correlation of Residuals of High/Low")

Open_Spec <- spec.pgram(diff_Open,spans=c(50,50), plot = TRUE, , main="Spectral Distribution of Bitcoin Historical Opening Prices")
Close_Spec <- spec.pgram(diff_Close,spans=c(50,50), plot = TRUE, main="Spectral Distribution of Bitcoin Historical Closing Prices")
High_Spec <- spec.pgram(diff_High,spans=c(50,50), plot = TRUE, main="Spectral Distribution of Bitcoin Historical High Prices")
Low_Spec <- spec.pgram(diff_Low,spans=c(50,50), plot = TRUE, main="Spectral Distribution of Bitcoin Historical Low Prices")

N = length(diff_Close)

A = c(0,1)
B = c(0,0,1)

Open_1 =  filter(diff_Open,A,sides=1)
Open_2 =  filter(diff_Open,B,sides=1)
Close_1 =  filter(diff_Close,A,sides=1)
Close_2 =  filter(diff_Close,B,sides=1)
High_1 =  filter(diff_High,A,sides=1)
High_2 = filter(diff_High,B,sides=1)
Low_1 =  filter(diff_Low,A,sides=1)
Low_2 =  filter(diff_Low,B,sides=1)
Volume_1 = filter(diff_Volume,A,sides=1)
Volume_2 = filter(diff_Volume,B,sides=1)

mod_1 = lm(diff_Close[3:length(diff_Close)]~High_1[3:length(High_1)]+Low_1[3:length(Low_1)])
summary(mod_1)
AIC(mod_1)
BIC(mod_1)

mod_2 = lm(diff_Close[3:length(diff_Close)]~Open_1[3:length(Open_1)]+High_1[3:length(High_1)]+Low_1[3:length(Low_1)]+Volume_1[3:length(Volume_1)])
summary(mod_2)
AIC(mod_2)
BIC(mod_2)

mod_3 = lm(diff_Close[3:length(diff_Close)]~Open_1[3:length(Open_1)]+Open_2[3:length(Open_2)]+High_1[3:length(High_1)]+High_2[3:length(High_2)]+Low_1[3:length(Low_1)]+Low_2[3:length(Low_2)]+Volume_1[3:length(Volume_1)]+Volume_2[3:length(Volume_2)])
summary(mod_3)
AIC(mod_3)
BIC(mod_3)

mod_1.resid = mod_1$residuals
plot(mod_1.resid)

acf(mod_1.resid, 10, main="output")
Box.test(mod_1.resid,2)

spectrum(mod_1.resid,log="no")
H = spectrum(mod_1.resid,spans=c(20,20))

ypred2=predict(mod_1)
plot(diff_Close[3:N],ypred2,xlab = 'observed', ylab = 'predicted')

Open_a <- diff_Open-mean(diff_Open)
Open_a = ts(Open_a,start=1,end=N)
Close_a <- diff_Close-mean(diff_Close)
Close_a = ts(Close_a,start=1,end=N)
High_a <- diff_High-mean(diff_High)
High_a = ts(High_a,start=1,end=N)
Low_a <- diff_Low-mean(diff_Low)
Low_a = ts(Low_a,start=1,end=N)
Vol_a <- diff_Vol-mean(diff_Vol)
Vol_a = ts(Vol_a, start=1, end=N)

Open_a1 = lag(Open_a,-1)
Close_a1 = lag(Close_a,-1)
High_a1 = lag(High_a,-1)
Low_a1 = lag(Low_a,-1)

Open_a2 = lag(Open_a1,-1)
Close_a2 = lag(Close_a1,-1)
High_a2 = lag(High_a1,-1)
Low_a2 = lag(Low_a1,-1)

N = length(Close_a)
N1 = N - 10
N2 = (N1+1):N

X1 = cbind(Open_a1, Close_a1, High_a1, Low_a1)
X2 = cbind(Open_a1, Open_a2, High_a1, High_a1, Low_a1, Low_a2)

Closef = Close_a[N2]
X1f = X1[N2,]
X2f = X2[N2,]

Close_a = Close_a[1:N1]
X1 = X1[1:N1,]
X2 = X2[1:N1,]

mod_1a = arima(Close_a, xreg=X1, order=c(0,0,1))
mod_1a

AIC(mod_1a)
BIC(mod_1a)

mod_2a = arima(Close_a, xreg=X2, order=c(1,0,0))
mod_2a

AIC(mod_2a)
BIC(mod_2a)

mod_3a = arima(Close_a, order = c(3,0,3))
mod_3a

AIC(mod_3a)
BIC(mod_3a)

cRes1 = residuals(mod_1a)
plot(cRes1)

acf(cRes1[3:N1], 10, main="output")

Box.test(cRes1,lag=5)
spectrum(cRes1[3:N1],log='no')

ns=10
yc=predict(mod_1a,newxreg=X1f[1:ns,])
yclose = yc$pred
ts.plot(Closef[1:ns], yclose, lty=1:2)

yc=predict(mod_2a,newxreg=X2f[1:ns,])
yclose = yc$pred
ts.plot(Closef[1:ns], yclose, lty=1:2)

yc=predict(mod_3a,n.ahead=ns)
yclose = yc$pred
ts.plot(Closef[1:ns], yclose, lty=1:2)