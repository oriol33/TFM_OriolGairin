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


#####################
#   Import data     #
#####################

#Germany CISS
germany <- read_csv("germany.csv", skip = 5)
germany <- germany[,1:2]
colnames(germany) <- c("date","value")
germany$date <- as.Date(germany$date)
germany_ts <- xts(germany$value, germany$date)
plot(germany_ts, type="l", col="darkblue", main="germany CISS")

#France CISS
france <- read_csv("france.csv", skip = 5)
france <- france[,1:2]
colnames(france) <- c("date","value")
france$date <- as.Date(france$date)
france_ts <- xts(france$value, france$date)
plot(france_ts, type="l", col="darkblue", main="france CISS")

#Italy CISS
italy <- read_csv("italy.csv", skip = 5)
italy <- italy[,1:2]
colnames(italy) <- c("date","value")
italy$date <- as.Date(italy$date)
italy_ts <- xts(italy$value, italy$date)
plot(italy_ts, type="l", col="darkblue", main="italy CISS")

#Portugal CISS
portugal <- read_csv("portugal.csv", skip = 5)
portugal <- portugal[,1:2]
colnames(portugal) <- c("date","value")
portugal$date <- as.Date(portugal$date)
portugal_ts <- xts(portugal$value, portugal$date)
plot(portugal_ts, type="l", col="darkblue", main="portugal CISS")

#UK CISS
uk <- read_csv("uk.csv", skip = 5)
uk <- uk[,1:2]
colnames(uk) <- c("date","value")
uk$date <- as.Date(uk$date)
uk_ts <- xts(uk$value, uk$date)
plot(uk_ts, type="l", col="darkblue", main="uk CISS")

#Spain CISS
spain <- read_csv("spain.csv", skip = 5)
spain <- spain[,1:2]
colnames(spain) <- c("date","value")
spain$date <- as.Date(spain$date)
spain_ts <- xts(spain$value, spain$date)
plot(spain_ts, type="l", col="darkblue", main="spain CISS")

#EU CISS
euro <- read_csv("euro.csv", skip = 5)
euro <- euro[,1:2]
colnames(euro) <- c("date","value")
euro$date <- as.Date(euro$date)
euro_ts <- xts(euro$value, euro$date)
plot(euro_ts, type="l", col="darkblue", main="euro CISS")

#US CISS
us <- read_csv("us.csv", skip = 5)
us <- us[,1:2]
colnames(us) <- c("date","value")
us$date <- as.Date(us$date)
us_ts <- xts(us$value, us$date)
plot(us_ts, type="l", col="darkblue", main="us CISS")

#China CISS
china <- read_csv("china.csv", skip = 5)
china <- china[,1:2]
colnames(china) <- c("date","value")
china$date <- as.Date(china$date)
china_ts <- xts(china$value, china$date)
plot(china_ts, type="l", col="darkblue", main="china CISS")

#Na's ? (NO)
sum(is.na(c(china_ts,us_ts,euro_ts,spain_ts,uk_ts,portugal_ts,italy_ts,france_ts,germany_ts)))



#Validation function (model for the mean) 
###########################################################################
validation=function(model,dades){
  s=frequency(get(model$series))
  resid=model$residuals
  par(mfrow=c(2,2),mar=c(3,3,3,3))
  #Residuals plot
  plot(resid,main="Residuals")
  abline(h=0)
  abline(h=c(-3*sd(resid),3*sd(resid)),lty=3,col=4)
  #Square Root of absolute values of residuals (Homocedasticity)
  scatter.smooth(sqrt(abs(resid)),main="Square Root of Absolute residuals",
                 lpars=list(col=2))
  
  #Normal plot of residuals
  qqnorm(resid)
  qqline(resid,col=2,lwd=2)
  
  ##Histogram of residuals with normal curve
  hist(resid,breaks=20,freq=FALSE)
  curve(dnorm(x,mean=mean(resid),sd=sd(resid)),col=2,add=T)
  
  
  #ACF & PACF of residuals
  par(mfrow=c(1,2))
  acf(resid,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=1)
  pacf(resid,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=1)
  par(mfrow=c(1,1))
  
  #ACF & PACF of square residuals 
  par(mfrow=c(1,2))
  acf(resid^2,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=1)
  pacf(resid^2,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=1)
  par(mfrow=c(1,1))
  
  #Ljung-Box p-values
  par(mar=c(2,2,1,1))
  tsdiag(model,gof.lag=7*s)
  cat("\n--------------------------------------------------------------------\n")
  print(model)
  
  #Stationary and Invertible
  cat("\nModul of AR Characteristic polynomial Roots: ", 
      Mod(polyroot(c(1,-model$model$phi))),"\n")
  cat("\nModul of MA Characteristic polynomial Roots: ",
      Mod(polyroot(c(1,model$model$theta))),"\n")
  
  #Model expressed as an MA infinity (psi-weights)
  psis=ARMAtoMA(ar=model$model$phi,ma=model$model$theta,lag.max=36)
  names(psis)=paste("psi",1:36)
  cat("\nPsi-weights (MA(inf))\n")
  cat("\n--------------------\n")
  print(psis[1:20])
  
  #Model expressed as an AR infinity (pi-weights)
  pis=-ARMAtoMA(ar=-model$model$theta,ma=-model$model$phi,lag.max=36)
  names(pis)=paste("pi",1:36)
  cat("\nPi-weights (AR(inf))\n")
  cat("\n--------------------\n")
  print(pis[1:20])
  
  #Sample ACF vs. Teoric ACF
  par(mfrow=c(2,2),mar=c(3,3,3,3))
  acf(dades, ylim=c(-1,1) ,lag.max=36,main="Sample ACF")
  
  plot(ARMAacf(model$model$phi,model$model$theta,lag.max=36),ylim=c(-1,1), 
       type="h",xlab="Lag",  ylab="", main="ACF Teoric")
  abline(h=0)
  
  #Sample PACF vs. Teoric PACF
  pacf(dades, ylim=c(-1,1) ,lag.max=36,main="Sample PACF")
  
  plot(ARMAacf(model$model$phi,model$model$theta,lag.max=36, pacf=T),ylim=c(-1,1),
       type="h", xlab="Lag", ylab="", main="PACF Teoric")
  abline(h=0)
  par(mfrow=c(1,1))
}
###########################################################################


                                         ##################################
                                         #           EURO CISS            #      
                                         ##################################

# Work with differencied series
d1euro_ts <- diff(euro_ts)
d1euro_ts <- d1euro_ts[-1]
plot(d1euro_ts)

# Check overdifferentation
var(euro_ts)
var(d1euro_ts)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1euro_ts, type = c("none"), selectlags = c("BIC"))
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1euro_ts,ylim=c(-0.1,1),main="d1serie")
pacf(d1euro_ts,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: AR(5)
(model = arima(d1euro_ts, order = c(5,0,0),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1euro_ts)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1euro_ts)
m1 
plot(m1)
#Observations: Model can be validated


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1euro_ts)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Reaction to news coef. (symmetric and asymmetric) not significant. Not the best.


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1euro_ts)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Best fitting model. Slightly asymmetric. 
#Validation is correct (but first lag of ACF residuals is significant)


##############################
###  Final model proposed: ###
##############################

#ARMA(4,0)-EGARCH(1,1) t-Student
#Observations: Validation is not 100% perfect, but overall best fitting model. Captures asymmetric behavior.
#We remove 5th AR component because loses significance when estimating variance.
m3
plot(m3)



                                         ##################################
                                         #            US CISS             #      
                                         ##################################

# Work with differencied series
plot(us_ts)
d1us_ts <- diff(us_ts)
d1us_ts <- d1us_ts[-1]
plot(d1us_ts)

# Check overdifferentation
var(us_ts)
var(d1us_ts)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1us_ts, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1us_ts,ylim=c(-0.1,1),main="d1serie")
pacf(d1us_ts,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(1,1)
(model = arima(d1us_ts, order = c(1,0,1),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1us_ts)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1us_ts)
m1 
plot(m1)
#Observations: Best validation !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1us_ts)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Almost equal to m1. Better m1


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1us_ts)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: NO significance asymmetric coef. Best fit. 


##############################
###  Final model proposed: ###
##############################

#ARMA(1,1)-GARCH(1,1) t-Student
#Observations: No asymmetric behaviour observed. Volatility is very sensitive to market events.
m1
plot(m1)



                                        ##################################
                                        #         China CISS             #      
                                        ##################################


# Work with differencied series
plot(china_ts)
d1china_ts <- diff(china_ts)
d1china_ts <- d1china_ts[-1]
plot(d1china_ts)

# Check overdifferentation
var(china_ts)
var(d1china_ts)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1china_ts, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1china_ts,ylim=c(-0.1,1),main="d1serie")
pacf(d1china_ts,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(0,5)
(model = arima(d1china_ts, order = c(0,0,5),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1china_ts)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,5)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1china_ts)
m1 
plot(m1)
#Observations: Validation problem for Standarized residuals. Rest is good.


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,5)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1china_ts)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Better fit, asymmetric behavior. ACF residuals problem (first lag).


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,5)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1china_ts)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Best fit. Validation not good. Does not capture volatility good enough (ACF squared residuals).


##############################
###  Final model proposed: ###
##############################

#ARMA(0,5)-gjrGARCH(1,1) t-Student
#Observations: Good fit. Asymmetric behavior. ACF or residuals not perfect.
m2
plot(m2)



                                        ##################################
                                        #        Germany CISS            #      
                                        ##################################

# Work with differencied series
plot(germany_ts)
d1serie <- diff(germany_ts)
d1serie <- d1serie[-1]
plot(d1serie)

# Check overdifferentation
var(germany_ts)
var(d1serie)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1serie, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1serie,ylim=c(-0.1,1),main="d1serie")
pacf(d1serie,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(5,0)
(model = arima(d1serie, order = c(5,0,0),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1serie)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1serie)
m1 
plot(m1)
#Observations: Good Validation !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1serie)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Good validation! Captures asymmetry


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1serie)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Worst fit.


##############################
###  Final model proposed: ###
##############################

#ARMA(4,0)-gjrGARCH(1,1) t-Student
#Observations: Good fit. Asymmetric behavior. Good Validation !
m2
plot(m2)



                                        ##################################
                                        #         France CISS            #      
                                        ##################################

# Work with differencied series
plot(france_ts)
d1serie <- diff(france_ts)
d1serie <- d1serie[-1]
plot(d1serie)

# Check overdifferentation
var(france_ts)
var(d1serie)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1serie, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1serie,ylim=c(-0.1,1),main="d1serie")
pacf(d1serie,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(4,0)
(model = arima(d1serie, order = c(4,0,0),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1serie)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1serie)
m1 
plot(m1)
#Observations: Good validation !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1serie)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Better fit. Good Val!


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1serie)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: It does not converge well... 


##############################
###  Final model proposed: ###
##############################

#ARMA(4,0)-gjrGARCH(1,1) t-Student
#Observations: Good fit. Asymmetric behavior. Good Validation !
m2
plot(m2)

                                        
                                        ##################################
                                        #         Italy CISS             #      
                                        ##################################

# Work with differencied series
plot(italy_ts)
d1serie <- diff(italy_ts)
d1serie <- d1serie[-1]
plot(d1serie)

# Check overdifferentation
var(italy_ts)
var(d1serie)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1serie, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1serie,ylim=c(-0.1,1),main="d1serie")
pacf(d1serie,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(1,1)
(model = arima(d1serie, order = c(1,0,1),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1serie)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1serie)
m1 
plot(m1)
#Observations: Good Val !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1serie)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Good val! slightly asymmetric


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1serie)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Best fit, good validation! captures asymmetry

#News-Impact curve
ni=newsimpact(z = NULL, m3)
plot(ni$zx, ni$zy, ylab=ni$yexpr, xlab=ni$xexpr, xlim=c(-0.07,0.07), type="l", main = "News Impact Curve")


##############################
###  Final model proposed: ###
##############################

#ARMA(1,1)-EGARCH(1,1) t-Student 
#Observations: good validation! captures asymmetry
m3
plot(m3)


                                        ##################################
                                        #           Portugal CISS        #      
                                        ##################################

# Work with differencied series
plot(portugal_ts)
d1serie <- diff(portugal_ts)
d1serie <- d1serie[-1]
plot(d1serie)

# Check overdifferentation
var(portugal_ts)
var(d1serie)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1serie, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1serie,ylim=c(-0.1,1),main="d1serie")
pacf(d1serie,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(1,1)
(model = arima(d1serie, order = c(1,0,1),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1serie)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1serie)
m1 
plot(m1)
#Observations: Good Val !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1serie)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Good val! slightly asymmetric


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1serie)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Best fit, Good Validation, but News Impact curve diverge. Should it be used as a good model?


##############################
###  Final model proposed: ###
##############################

#ARMA(1,1)-gjrGARCH(1,1) t-Student 
#Observations: Egarch could be used instead, but maybe it has problems of convergence? (news impact curve)
m2
plot(m2)


                                        ##################################
                                        #             UK CISS            #      
                                        ##################################

# Work with differencied series
plot(uk_ts)
d1serie <- diff(uk_ts)
d1serie <- d1serie[-1]
plot(d1serie)

# Check overdifferentation
var(uk_ts)
var(d1serie)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1serie, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1serie,ylim=c(-0.1,1),main="d1serie")
pacf(d1serie,ylim=c(-0.1,1),main="d1serie")
# There appears to be a strong autocorrelation structure around lag 30

#Proposed model for the mean: ARMA(1,1)
(model = arima(d1serie, order = c(1,0,1),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1serie)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1serie)
m1 
plot(m1)
#Observations: Good Val !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1serie)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: gamma1 is non-significant.


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1serie)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Best fit, but Validation on Residuals is not clear (autocorrelation).


##############################
###  Final model proposed: ###
##############################

#ARMA(1,1)-GARCH(1,1) t-Student 
#Observations: Asymmetry is not clear in this case and Egarch presents validations problems.
# There appears to be a strong autocorrelation structure around lag 30, that complicates things.
m1
plot(m1)


                                        ##################################
                                        #          Spain CISS            #      
                                        ##################################

# Work with differencied series
plot(spain_ts)
d1serie <- diff(spain_ts)
d1serie <- d1serie[-1]
plot(d1serie)

# Check overdifferentation
var(spain_ts)
var(d1serie)

# Check stationarity  (#H0: serie is I(1))
d1serie.df<-ur.df(d1serie, type = c("none"), lags = 3)
summary(d1serie.df)
plot(d1serie.df)

#Identification (model for the mean)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(d1serie,ylim=c(-0.1,1),main="d1serie")
pacf(d1serie,ylim=c(-0.1,1),main="d1serie")

#Proposed model for the mean: ARMA(3,0)
(model = arima(d1serie, order = c(3,0,0),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#Validation
validation(model,d1serie)
#Clear signs of volatility


## Model for the variance ##

#ARMA-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(3,0)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=d1serie)
m1 
plot(m1)
#Observations: Good validation !


#ARMA-GJR-GARCH t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(3,0)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=d1serie)
m2
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye, respecto a diferencias positivas.
plot(m2)
#Observations: Better fit! Captures asymmetry. Better Validation!


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(3,0)), distribution.model = "std")
m3=ugarchfit(spec=spec3,data=d1serie)
m3
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si la diferencia fuera positiva.
plot(m3)
#Observations: Convergence problems !! Can't use it (but it's best fit...)


##############################
###  Final model proposed: ###
##############################

#ARMA(3,0)-gjrGARCH(1,1) t-Student
#Observations: Captures asymmetry. Better Validation.
m2
plot(m2)














