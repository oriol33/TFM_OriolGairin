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


##############################################
#               FORECASTS                    #
##############################################

#Partitions for Train + Test
#Validation is done from econometric p.o.v.
train_pct <- 0.9
test_pct <- 1-train_pct

##### Univariate Forecasts ###################

##### Germany ################################

# We fit the model on differenced data, then we inverse transform.
d1germany_ts <- diff(germany_ts)
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(4,0)-gjrGARCH(1,1) t-StudentFrance
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+
        forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
        forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
     mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
    (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))








##### France ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- france_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(france_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(4,0)-gjrGARCH(1,1) t-Student
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+
  forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))




##### Italy ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- italy_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(1,1)-EGARCH(1,1) t-Student
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+
  forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))




##### Portugal ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- portugal_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set


#ARMA(1,1)-gjrGARCH(1,1) t-Student 
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))





##### UK ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- uk_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(1,1)-GARCH(1,1) t-StudentSpain 
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))




##### Spain ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- spain_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(3,0)-gjrGARCH(1,1) t-Student 
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(3,0)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))




##### EU ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- euro_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(4,0)-EGARCH(1,1) t-Student
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))





##### US ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- us_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- round(test_pct*length(d1germany_ts),0) #Observations that we leave outside the training set

#ARMA(1,1)-GARCH(1,1) t-Student
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (round(train_pct*length(germany_ts),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (round(train_pct*length(ts_naive),0)+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))





##### China ################################

# We fit the model on differenced data, then we inverse transform.
germany_ts <- china_ts            # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- diff(germany_ts)    # !!!!! BE CAREFUL ################################################## !!!!
d1germany_ts <- d1germany_ts[-1]

out_sample <- 575 #Observations that we leave outside the training set

#ARMA(0,5)-gjrGARCH(1,1) t-Student
# We fit the model, leaving out the test data
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,5)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts, out.sample = out_sample )

#FORECAST y(t+1|t)
ugarchforecast(m_germany, n.ahead=10, n.roll= 0) 
forc_germany_1 = ugarchforecast(m_germany, n.ahead=1, n.roll= out_sample) 

fpm(forc_germany_1) #Forecast performance measures
sigma(forc_germany_1)  #Forecast standard deviation
fitted(forc_germany_1) #Forecast time series (point prediction)
#plot(forc_germany_1)   #Plot forecast, with confidence intervals


####   FORECAST y(t+1 .... t+10 |t)     #########
forc_germany = ugarchforecast(m_germany, n.ahead=10, n.roll= out_sample)

forc_germany_sigma_TOT <- t(as.data.frame(sigma(forc_germany))) 
forc_germany_mean_TOT <- t(as.data.frame(fitted(forc_germany))) 

######  T+1|T  #####

#Horizon
H <- 1

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1])

#First day of forecast
F1 <- (3585+H)

#Select forecast dates
dates <- rev(china$date)
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('pred_diff', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$pred_diff+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff) #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$pred_diff)

#The metrics are invariante with respect to diff() trasnformation.




######  T+5|T  ########

#Horizon
H <- 5

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (3585+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4', 'T5', 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T3)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T3)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps


######  T+10|T  ########

#Horizon
H <- 10

#Substract H last predictions, as we don't have data to compare.
forc_germany_mean <- forc_germany_mean_TOT[1:(dim(forc_germany_mean_TOT)[1]-H),]

#Select T+H forecast
forc_germany_mean <- as.data.frame(forc_germany_mean[,1:H])

#First day of forecast
F1 <- (3585+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

#Build dataset with dates, actual values and predicted values
forc_germany_mean <- cbind(forc_germany_mean,dates_f) #date

forc_germany_mean <- cbind(forc_germany_mean, d1germany_ts[(F1-1):length(d1germany_ts)]) #actual_diff
forc_germany_mean <- cbind(forc_germany_mean,germany_ts[F1:length(germany_ts)]) #actual

lagged_actual <- germany_ts[(F1-H):(length(germany_ts)-H)] #lagged H times
forc_germany_mean <- cbind(forc_germany_mean,lagged_actual)

#rownames(forc_germany_mean) <- NULL
colnames(forc_germany_mean) <- c('T1','T2', 'T3', 'T4','T5', 'T6', 'T7','T8', 'T9', 'T10', 
                                 'date', 'actual_diff', 'actual', 'lagged_actual')

#We undo de diff() transformation. We get the prediction for the time series
pred <- forc_germany_mean$T1+forc_germany_mean$T2+forc_germany_mean$T3+forc_germany_mean$T4+forc_germany_mean$T5+forc_germany_mean$T6+
  forc_germany_mean$T7+forc_germany_mean$T8+forc_germany_mean$T9+forc_germany_mean$T10+forc_germany_mean$lagged_actual
forc_germany_mean <- cbind(forc_germany_mean, pred)

#Plot the results
plot(forc_germany_mean$actual)
lines(forc_germany_mean$pred)

#Compute forecast metrics

#MSE
mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred)   #original series 
mean((forc_germany_mean$actual - forc_germany_mean$pred)^2)                  #original series

mse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10) #differenced series
mean((forc_germany_mean$actual_diff - forc_germany_mean$T10)^2)                #differenced series

#RMSE
rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
rmse(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#MAE
mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) 
mae(actual = forc_germany_mean$actual_diff, predicted = forc_germany_mean$T10)

#Here, the metric CHANGES with respect to the transformation !!!
#Because we have to add the forecasting errors of previous timesteps



####### NAIVE APROXIMATION x_{t-1} = x_t #######

# T+H|T

#Horizon
H <- 10
#Time series
ts_naive <- array(germany_ts)

#First day of forecast
F1 <- (3585+H)

#Select forecast dates
dates_f <- dates[F1:length(dates)]

naive_aprox <- data.frame(matrix(ncol = 3, nrow = length(dates_f)))
colnames(naive_aprox) <- c('dates_f', 'actual', 'lagged_H')

naive_aprox$dates_f <- dates_f
naive_aprox$actual <- ts_naive[F1:length(ts_naive)]
naive_aprox$lagged_H <- ts_naive[(F1-H):(length(ts_naive)-H)]

#Plot the results
plot(naive_aprox$actual)
lines(naive_aprox$lagged_H)

#MSE
mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#RMSE
rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)

#MAE
mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H)


###### RELATIVE GAIN (ARMA-GARCH models vs. Naive approach) #######

#MSE gain
-100*(mse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#RMSE gain
-100*(rmse(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (rmse(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))

#MAE
-100*(mae(actual = forc_germany_mean$actual, predicted = forc_germany_mean$pred) -
        mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H) )/
  (mae(actual = naive_aprox$actual, predicted = naive_aprox$lagged_H))











