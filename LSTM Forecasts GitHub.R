####################
#   Load Packages  #
####################

library(tseries)
library(fBasics)
library(car)
library(urca)
library(forecast)
library(fGarch)
library(rugarch)
library(quantmod)
library(ggplot2)
library(fTrading)
library(rmgarch)
library(readr)
library(xts)
library(vars)
library(Spillover)
library(tidyverse)

library(Metrics)


#####################
#   Import data     #
#####################

#Germany CISS
germany <- read_csv("germany.csv", skip = 5)
germany <- germany[,1:2]
colnames(germany) <- c("date","value")
germany$date <- as.Date(germany$date)
germany_ts <- xts(germany$value, germany$date)
#plot(germany_ts, type="l", col="darkblue", main="germany CISS")

#France CISS
france <- read_csv("france.csv", skip = 5)
france <- france[,1:2]
colnames(france) <- c("date","value")
france$date <- as.Date(france$date)
france_ts <- xts(france$value, france$date)
#plot(france_ts, type="l", col="darkblue", main="france CISS")

#Italy CISS
italy <- read_csv("italy.csv", skip = 5)
italy <- italy[,1:2]
colnames(italy) <- c("date","value")
italy$date <- as.Date(italy$date)
italy_ts <- xts(italy$value, italy$date)
#plot(italy_ts, type="l", col="darkblue", main="italy CISS")

#Portugal CISS
portugal <- read_csv("portugal.csv", skip = 5)
portugal <- portugal[,1:2]
colnames(portugal) <- c("date","value")
portugal$date <- as.Date(portugal$date)
portugal_ts <- xts(portugal$value, portugal$date)
#plot(portugal_ts, type="l", col="darkblue", main="portugal CISS")

#UK CISS
uk <- read_csv("uk.csv", skip = 5)
uk <- uk[,1:2]
colnames(uk) <- c("date","value")
uk$date <- as.Date(uk$date)
uk_ts <- xts(uk$value, uk$date)
#plot(uk_ts, type="l", col="darkblue", main="uk CISS")

#Spain CISS
spain <- read_csv("spain.csv", skip = 5)
spain <- spain[,1:2]
colnames(spain) <- c("date","value")
spain$date <- as.Date(spain$date)
spain_ts <- xts(spain$value, spain$date)
#plot(spain_ts, type="l", col="darkblue", main="spain CISS")

#EU CISS
euro <- read_csv("euro.csv", skip = 5)
euro <- euro[,1:2]
colnames(euro) <- c("date","value")
euro$date <- as.Date(euro$date)
euro_ts <- xts(euro$value, euro$date)
#plot(euro_ts, type="l", col="darkblue", main="euro CISS")

#US CISS
us <- read_csv("us.csv", skip = 5)
us <- us[,1:2]
colnames(us) <- c("date","value")
us$date <- as.Date(us$date)
us_ts <- xts(us$value, us$date)
#plot(us_ts, type="l", col="darkblue", main="us CISS")

#China CISS
china <- read_csv("china.csv", skip = 5)
china <- china[,1:2]
colnames(china) <- c("date","value")
china$date <- as.Date(china$date)
china_ts <- xts(china$value, china$date)
#plot(china_ts, type="l", col="darkblue", main="china CISS")

#Na's ? (NO)
sum(is.na(c(china_ts,us_ts,euro_ts,spain_ts,uk_ts,portugal_ts,italy_ts,france_ts,germany_ts)))

#Get the dates
dates <- rev(euro$date)


####################################
#       Load LSTM Forecasts        #
####################################


#Predictions for t+1|t. Every country is every 510 observations
library(readxl)
actual <- read_excel("actual.xlsx")
pred <- read_excel("pred.xlsx") 

ts.actual <- ts(actual)
ts.pred <- ts(pred)
ts.error <- ts(ts.actual-ts.pred)

plot(ts.actual)
plot(ts.pred)
plot(ts.error)

#Choose one country: UK
position <- 2

ts.actual.spain <- ts.actual[(510*(position-1)):(510*(position-1)+510)]
ts.actual.spain <- ts.actual.spain[-1]

ts.pred.spain <- ts.pred[(510*(position-1)):(510*(position-1)+510)]
ts.pred.spain <- ts.pred.spain[-1]

ts.error.spain <- ts.error[(510*(position-1)):(510*(position-1)+510)]
ts.error.spain <- ts.error.spain[-1]

#ARMA-GARCH Forecast
#UK_arma_t1 <- forc_germany_mean[(dim(forc_germany_mean)[1]-(510-1)):dim(forc_germany_mean)[1],]
#Naive Forecast
#UK_naive_t1 <- naive_aprox[(dim(naive_aprox)[1]-(510-1)):dim(naive_aprox)[1],]

#Plots

par(family = "serif")

plot(ts.pred.spain, xlab='Time', ylab='Forecast t+1|t')
lines(ts.actual.spain)
lines(UK_arma_t1$pred, type='p', col='blue')
lines(UK_naive_t1$lagged_H, type='p', col='green')
legend("topleft", c("LSTM","ARMA","Naive"), pch =c(1,1,1), col=c('black', 'blue', 'green'), cex=0.6)


#data <- as.data.frame(ts.pred.spain)
#dim(data)
#dates2 <- dates[(length(dates)-510+1):length(dates)]
#length(dates2)
#dim(data)
#data$day <- dates2

p1 <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = ts.actual.spain), color="black", size = 1, alpha=0.6) + 
  geom_point(aes(y = ts.pred.spain), shape=20, color="#D53E4F", size=3) +
  geom_point(aes(y = UK_arma_t1$pred), shape=20, color="#FEE08B", size=3) +
  geom_point(aes(y = UK_naive_t1$lagged_H), shape=20, color="#3288BD", size=3) +
  labs(x = "Time", y = "Forecast (t+1|t)")+ylim(0,0.5)+
  theme(text = element_text(family = "serif", size=14))
p1

plot(ts.error.spain, t= "l", col='black')
abline(h = 0, lty = "dashed", col = "black")
#lines((UK_arma_t1$actual-UK_arma_t1$pred), type='l', col='blue')
#lines((UK_naive_t1$actual-UK_naive_t1$lagged_H), type='l', col='green')

acf(ts.error.spain)
acf(ts.error.spain^2)

plot(as.data.frame(ts.actual.spain)[,1],as.data.frame(ts.pred.spain)[,1])
abline(coef = c(0,1))

plot(as.data.frame(ts.pred.spain)[,1], as.data.frame(ts.actual.spain)[,1], xlab='Forecast t+1|t', ylab='Actual value')
points(UK_arma_t1$pred, UK_arma_t1$actual, col = "blue")
points(UK_naive_t1$lagged_H, UK_naive_t1$actual, col = "green")
abline(coef = c(0,1))
legend("topleft", c("LSTM","ARMA","Naive"), pch =c(1,1,1), col=c('black', 'blue', 'green'), cex=0.6)


p2 <- ggplot(data, aes(x=day)) +
  geom_abline(intercept = 0, slope = 1, color="black", size = 1, alpha=0.6)+
  geom_point(aes(y= ts.actual.spain, x= ts.pred.spain), shape=20, color="#D53E4F", size=3) +
  geom_point(aes(y= ts.actual.spain, x=UK_arma_t1$pred), shape=20, color="#FEE08B", size=3) +
  geom_point(aes(y= ts.actual.spain, x=UK_naive_t1$lagged_H), color="#3288BD", shape=20, size=3) +
  labs(x = "Forecast (t+1|t)", y = "Actual")+ylim(0,0.5)+
  theme(text = element_text(family = "serif", size=14))
p2

grid.arrange(p1, p2, nrow = 1)


#Predictions for t+5|t. Every country is every 506 observations
library(readxl)
actual <- read_excel("actualt5.xlsx")
pred <- read_excel("predt5.xlsx") 


ts.actual <- ts(actual)
ts.pred <- ts(pred)
ts.error <- ts(ts.actual-ts.pred)

plot(ts.actual)
plot(ts.pred)
plot(ts.error)

#Choose one country: UK
position <- 2

ts.actual.spain <- ts.actual[(506*(position-1)):(506*(position-1)+506)]
ts.actual.spain <- ts.actual.spain[-1]

ts.pred.spain <- ts.pred[(506*(position-1)):(506*(position-1)+506)]
ts.pred.spain <- ts.pred.spain[-1]

ts.error.spain <- ts.error[(506*(position-1)):(506*(position-1)+506)]
ts.error.spain <- ts.error.spain[-1]

#ARMA-GARCH Forecast
#UK_arma_t5 <- forc_germany_mean[(dim(forc_germany_mean)[1]-(506-1)):dim(forc_germany_mean)[1],]
#Naive Forecast
#UK_naive_t5 <- naive_aprox[(dim(naive_aprox)[1]-(506-1)):dim(naive_aprox)[1],]

#Plots
plot(ts.pred.spain, xlab='Time', ylab='Forecast t+5|t')
lines(ts.actual.spain)
lines(UK_arma_t5$pred, type='p', col='blue')
lines(UK_naive_t5$lagged_H, type='p', col='green')
legend("topleft", c("LSTM","ARMA","Naive"), pch =c(1,1,1), col=c('black', 'blue', 'green'), cex=0.6)


plot(ts.error.spain, t= "l", col='black')
abline(h = 0, lty = "dashed", col = "black")
#lines((UK_arma_t1$actual-UK_arma_t1$pred), type='l', col='blue')
#lines((UK_naive_t1$actual-UK_naive_t1$lagged_H), type='l', col='green')

acf(ts.error.spain)
acf(ts.error.spain^2)

plot(as.data.frame(ts.actual.spain)[,1],as.data.frame(ts.pred.spain)[,1])
abline(coef = c(0,1))

plot(as.data.frame(ts.pred.spain)[,1], as.data.frame(ts.actual.spain)[,1], xlab='Forecast t+5|t', ylab='Actual value')
points(UK_arma_t5$pred, UK_arma_t5$actual, col = "blue")
points(UK_naive_t5$lagged_H, UK_naive_t5$actual, col = "green")
abline(coef = c(0,1))
legend("topleft", c("LSTM","ARMA","Naive"), pch =c(1,1,1), col=c('black', 'blue', 'green'), cex=0.6)


#data <- as.data.frame(ts.pred.spain)
#dim(data)
#dates2 <- dates[(length(dates)-506+1):length(dates)]
#length(dates2)
#dim(data)
#data$day <- dates2

p1 <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = ts.actual.spain), color="black", size = 1, alpha=0.6) + 
  geom_point(aes(y = ts.pred.spain), shape=20, color="#D53E4F", size=3) +
  geom_point(aes(y = UK_arma_t5$pred), shape=20, color="#FEE08B", size=3) +
  geom_point(aes(y = UK_naive_t5$lagged_H), shape=20, color="#3288BD", size=3) +
  labs(x = "Time", y = "Forecast (t+5|t)")+ylim(0,0.5)+
  theme(text = element_text(family = "serif", size=14))
p1

p2 <- ggplot(data, aes(x=day)) +
  geom_abline(intercept = 0, slope = 1, color="black", size = 1, alpha=0.6)+
  geom_point(aes(y= ts.actual.spain, x= ts.pred.spain), shape=20, color="#D53E4F", size=3) +
  geom_point(aes(y= ts.actual.spain, x=UK_arma_t5$pred), shape=20, color="#FEE08B", size=3) +
  geom_point(aes(y= ts.actual.spain, x=UK_naive_t5$lagged_H), color="#3288BD", shape=20, size=3) +
  labs(x = "Forecast (t+5|t)", y = "Actual")+ylim(0,0.5)+
  theme(text = element_text(family = "serif", size=14))
p2

grid.arrange(p1, p2, nrow = 1)


#Predictions for t+10|t. Every country is every 501 observations
library(readxl)
actual <- read_excel("actualt10.xlsx")
pred <- read_excel("predt10.xlsx") 


ts.actual <- ts(actual)
ts.pred <- ts(pred)
ts.error <- ts(ts.actual-ts.pred)

plot(ts.actual)
plot(ts.pred)
plot(ts.error)

#Choose one country: UK
position <- 2

ts.actual.spain <- ts.actual[(501*(position-1)):(501*(position-1)+501)]
ts.actual.spain <- ts.actual.spain[-1]

ts.pred.spain <- ts.pred[(501*(position-1)):(501*(position-1)+501)]
ts.pred.spain <- ts.pred.spain[-1]

ts.error.spain <- ts.error[(501*(position-1)):(501*(position-1)+501)]
ts.error.spain <- ts.error.spain[-1]

#ARMA-GARCH Forecast
#UK_arma_t10 <- forc_germany_mean[(dim(forc_germany_mean)[1]-(501-1)):dim(forc_germany_mean)[1],]
#Naive Forecast
#UK_naive_t10 <- naive_aprox[(dim(naive_aprox)[1]-(501-1)):dim(naive_aprox)[1],]

#Plots
plot(ts.pred.spain, xlab='Time', ylab='Forecast t+10|t')
lines(ts.actual.spain)
lines(UK_arma_t10$pred, type='p', col='blue')
lines(UK_naive_t10$lagged_H, type='p', col='green')
legend("topleft", c("LSTM","ARMA","Naive"), pch =c(1,1,1), col=c('black', 'blue', 'green'), cex=0.6)


plot(ts.error.spain, t= "l", col='black')
abline(h = 0, lty = "dashed", col = "black")
#lines((UK_arma_t1$actual-UK_arma_t1$pred), type='l', col='blue')
#lines((UK_naive_t1$actual-UK_naive_t1$lagged_H), type='l', col='green')

acf(ts.error.spain)
acf(ts.error.spain^2)

plot(as.data.frame(ts.actual.spain)[,1],as.data.frame(ts.pred.spain)[,1])
abline(coef = c(0,1))

plot(as.data.frame(ts.pred.spain)[,1], as.data.frame(ts.actual.spain)[,1], xlab='Forecast t+10|t', ylab='Actual value')
points(UK_arma_t10$pred, UK_arma_t10$actual, col = "blue")
points(UK_naive_t10$lagged_H, UK_naive_t10$actual, col = "green")
abline(coef = c(0,1))
legend("topleft", c("LSTM","ARMA","Naive"), pch =c(1,1,1), col=c('black', 'blue', 'green'), cex=0.6)


#data <- as.data.frame(ts.pred.spain)
#dim(data)
#dates2 <- dates[(length(dates)-501+1):length(dates)]
#length(dates2)
#dim(data)
#data$day <- dates2

p1 <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = ts.actual.spain), color="black", size = 1, alpha=0.6) + 
  geom_point(aes(y = ts.pred.spain), shape=20, color="#D53E4F", size=3) +
  geom_point(aes(y = UK_arma_t10$pred), shape=20, color="#FEE08B", size=3) +
  geom_point(aes(y = UK_naive_t10$lagged_H), shape=20, color="#3288BD", size=3) +
  labs(x = "Time", y = "Forecast (t+10|t)")+ylim(0,0.5)+
  theme(text = element_text(family = "serif", size=14))
p1

p2 <- ggplot(data, aes(x=day)) +
  geom_abline(intercept = 0, slope = 1, color="black", size = 1, alpha=0.6)+
  geom_point(aes(y= ts.actual.spain, x= ts.pred.spain), shape=20, color="#D53E4F", size=3) +
  geom_point(aes(y= ts.actual.spain, x=UK_arma_t10$pred), shape=20, color="#FEE08B", size=3) +
  geom_point(aes(y= ts.actual.spain, x=UK_naive_t10$lagged_H), color="#3288BD", shape=20, size=3) +
  labs(x = "Forecast (t+10|t)", y = "Actual")+ylim(0,0.5)+
  theme(text = element_text(family = "serif", size=14))
p2

grid.arrange(p1, p2, nrow = 1)



