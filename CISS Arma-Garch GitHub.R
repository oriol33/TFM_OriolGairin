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



                                         ##################################
                                         #           EURO CISS            #      
                                         ##################################


##################################
#    Analysis of stationarity    #      
##################################

intc <- euro_ts

# Informal Tools

#ACF-PACF
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1) 
acf(intc,ylim=c(-1,1),main="INTC") #Decae muy lentamente
pacf(intc,ylim=c(-1,1),main="INTC")

#Ljung-Box test
#H0: p primeros coeficientes de la función de autocorrelación son conjuntamente igual a 0.
Box.test(intc, lag = 1, type = c("Ljung-Box"))
Box.test(intc, lag = 5, type = c("Ljung-Box"))
Box.test(intc, lag = 10, type = c("Ljung-Box"))
Box.test(intc, lag = 15, type = c("Ljung-Box"))
Box.test(intc, lag = 20, type = c("Ljung-Box"))

#La serie NO es estacionaria según los criterios informales.

##################
#  Formal tools  #
##################

## Augmented Dickey-Fuller Test

#Serie:

# 4 lags + drift
#H0: La serie es I(1).
intc.df<-ur.df(intc, type = c("drift"), lags = 4)
summary(intc.df) 
plot(intc.df)  #Residuals are compatible with white noise

# Rechazamos H0. Concluimos que la serie ya es estacionaria I(0).

#Log-returns:

# Calculamos log-returns
rendintc <- diff(log(intc))
rendintc <- rendintc[-1]
plot(rendintc) # Clearly cte. mean and no trend. Variance not cte.

#BIC
rendintc.df<-ur.df(rendintc, type = c("none"), selectlags = c("BIC"))
summary(rendintc.df)
plot(rendintc.df)

# La serie diferenciada claramente es estacionaria, pues ya lo es su serie original.


## PP test

#H0: serie es I(1)
#H1: serie es I(0)

#Serie:

intc.pp<-ur.pp(intc, type = c("Z-tau"), model = c("constant"), lags = c("short"))
summary(intc.pp)  # Rechazamos H0, la serie ya es estacionaria

#Log-returns:

rendintc.pp<-ur.pp(rendintc, type = c("Z-tau"), model = c("constant"), lags = c("short"))
summary(rendintc.pp)
# La serie diferenciada claramente es estacionaria, pues ya lo es su serie original.


## KPSS test

#H0: la serie es trend stationary  "" I(0) ""
#H1: la serie no es trend stationary "" I(1) ""

# Serie:

intc.kpss<-ur.kpss(intc, type = c("mu"), lags = c("short"))
summary(intc.kpss)   #Rechazamos H0 -> la serie NO es trend stationary

# Log-returns: 

rendintc.kpss<-ur.kpss(rendintc, type = c("mu"), lags = c("short"))
summary(rendintc.kpss) #La serie diferenciada sí es trend stationary.


## CONCLUSIÓN ESTACIONARIEDAD : Diferenciamos para asegurar estacionariedad ##

# Las herramientas usadas no son concluyentes:
# Las herramientas informales y el KPSS test concluyen que la serie no es estacionaria (debemos diferenciar).
# ADF y PP test concluyen que la serie original ya es estacionaria.

# MI OPINIÓN: Ante la duda, yo diferenciaría para asegurar estacionariedad en todas las series.
# Así, 1: Mantenemos la consistencia para todas las series de estudio.
#      2: Identificamos más eficientemente un modelo arma para la media.
#      3: Aseguramos trend stationarity


# Calculamos log-returns
#rendintc <- diff(log(intc))
#rendintc <- rendintc[-1]
rendintc <- diff(intc)
rendintc <- rendintc[-1]
plot(rendintc) # Clearly cte. mean and no trend. Variance not cte.

# Al diferenciar, la varianza incondicional de la serie se reduce, lo que nos indica que NO estamos "sobrediferenciando".
var(intc)
var(rendintc)


##################################
#    Descriptive statistics      #      
##################################

normalTest(rendintc,method="jb")  # Reject Normality

basicStats(rendintc) 
# Positive skewness. Asimétrica hacia la derecha
# Positive kurtosis (Leptokurtic). Heavy tails. 

# Histogram of returns with normal curve
win.graph(width=8,height=5)
hist(rendintc,breaks=50,freq=F, main = 'Histogram of returns')
curve(dnorm(x, mean=mean(rendintc), sd=sd(rendintc)), col=2, add=T)

win.graph(width=8,height=5)
qqnorm(rendintc)
qqline(rendintc, datax = FALSE)
# Clearly heavy tailed, far from normal in the extremes.


##################################
#    Identification (media)      #      
##################################

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(rendintc,ylim=c(-0.1,1),main="returns")
pacf(rendintc,ylim=c(-0.1,1),main="returns")

#Candidates:
#AR(5): ACF decays exp. ; PACF 5 coef. different from 0
#ARMA(1,1): both coef. decay exponentially

##################################
#    Estimation (media)          #      
##################################

#MA(3) 
#(model = arima(rendintc, order = c(0,0,3),include.mean = TRUE))
#pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
#BIC(model)

#AR(5) 
(model = arima(rendintc, order = c(5,0,0),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#ARMA(1,1) 
(model = arima(rendintc, order = c(1,0,1),include.mean = TRUE))
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)
BIC(model)

#ARMA(1,1) or AR(5) more plausible
#AR(5) el mejor en la validación !

##################################
#    Validation (media)          #      
##################################

#################Validation######################################
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
################# Fi Validation #################################

validation(model,rendintc)

plot(model) #Inverse inside unit circle -> roots lie outside unit circle
# Es estacionario

# Resiaduals
windows()
tsdiag(model)  #jointly independent (up to lag 8)

#ACF & PACF
#Compatible with white noise
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(model$residuals,main="residuos")
pacf(model$residuals,ylim=c(-0.05,1),main="residuos")

#Heavy tails of residuals
qqnorm(model$residuals)
qqline(model$residuals, datax = FALSE)

plot(model$residuals)
title (main="Gráfico de los residuos")
normalTest(model$residuals,method="jb") # Rejects normality.

## Squared residuals ##

# ACF & PACF de los residuos al cuadrado 
residuos <- model$residuals
residuos2 <- residuos^2

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(residuos2,main="residuos al cuadrado") 
pacf(residuos2,ylim=c(-0.04,1),main="residuos al cuadrado")

Box.test(residuos2,lag=1,type='Ljung')
Box.test(residuos2,lag=5,type='Ljung')
Box.test(residuos2,lag=15,type='Ljung')
Box.test(residuos2,lag=20,type='Ljung')
#The residuals are jointly autocorrelated up to 20 lags (and more)


### CONCLUSIÓN DE LA VALIDACIÓN ###

# Podemos concluir que claramente existen efectos ARCH (efectos de volatilidad), es decir nuestro modelo es Heterocedástico.
# Respecto a las otras partes del modelo, los podemos como válido a falta de complementarlo con un modelo para la varianza.


##################################
#    Estimation (varianza)       #      
##################################

#ARMA-GARCH(1,1) t-Student
spec2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m2=ugarchfit(spec=spec2,data=rendintc)
m2 
# No joint autocorrelation for residuals^2 nor residuals
#t-Student ajustará mejor en general para nuestros datos.
plot(m2)
# El modelo se puede dar como válido.


#ARMA-GJR-GARCH t-Student
spec6=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m6=ugarchfit(spec=spec6,data=rendintc)
m6
#gamma<0 indica que cuando la diferencia es negativa (disminuye el systemic risk),
# la volatilidad de la serie ligeramente disminuye
#gamma estadísticamente no significativa
plot(m6)
# El modelo se puede dar como válido


#ARMA-EGARCH t-Student
#alpha1 se corresponde con el gamma de las diapositivas (coef. asimétrico)
#Gamma de R se corresponde con el theta de las diapositivas.
spec4=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m4=ugarchfit(spec=spec4,data=rendintc)
m4
# alpha1 positivo (coef. asim.) significa que para diferencia negativa (disminuye systemic risk), 
# la volatilidad del día siguiente disminuye respecto si el return fuera positivo.
plot(m4)



##################################
#    Validation (varianza)       #      
##################################

plot(m6)
#10: residuos estandarizados ACF
#11: ACF of Squared Standardized Residuals: NO autocorrelation
#8: Empirical Density of Standardized Residuals: Residuals clearly follow t-Student

#3: Conditional SD (vs |returns|): siguen la misma dinámica, a mayor variabilidad en los rendimientos, más volatilidad.
#12 News-Impact Curve: Se muestra el comportamiento asimétrico.
#2  Series with 1% VaR Limits

#Residuals obtention (alternative)
resi=residuals(m4,standardize=T) # Standardized residuals
par(mfcol=c(2,1)) # Obtain ACF & PACF
acf(resi,lag=24)
acf(resi^2,lag=24)










