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


##################################################################
#     Volatility_ts from estimated  ARMA-GARCH models            #      
##################################################################

                                            ### EURO ###
d1euro_ts <- diff(euro_ts)
d1euro_ts <- d1euro_ts[-1]
#plot(d1euro_ts)

#ARMA(4,0)-EGARCH(1,1) t-Student
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m_euro=ugarchfit(spec=spec3,data=d1euro_ts)
#m_euro
#plot(m_euro)

#Volatility series
voleuro = sigma(m_euro) 
voleuro_an=(252)^0.5*voleuro
#plot(voleuro_an)

#Volatility vs. CISS
#time = data.frame(euro_ts[-1], voleuro_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)


                                              ### US ###
d1us_ts <- diff(us_ts)
d1us_ts <- d1us_ts[-1]
#plot(d1us_ts)

#ARMA(1,1)-GARCH(1,1) t-Student
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_us=ugarchfit(spec=spec1,data=d1us_ts)
#m_us 
#plot(m_us)

#Volatility series
volus = sigma(m_us) 
volus_an=(252)^0.5*volus
#plot(volus_an)

#Volatility vs. CISS
#time = data.frame(us_ts[-1], volus_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)


                                          ### China ###
d1china_ts <- diff(china_ts)
d1china_ts <- d1china_ts[-1]
#plot(d1china_ts)

#ARMA(0,5)-gjrGARCH(1,1) t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,5)), distribution.model = "std")
m_china=ugarchfit(spec=spec2,data=d1china_ts)
#m_china
#plot(m_china)

#Volatility series
volchina = sigma(m_china) 
volchina_an=(252)^0.5*volchina
#plot(volchina_an)

#Volatility vs. CISS
#time = data.frame(china_ts[-1], volchina_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)


                                       ### Germany ###

d1germany_ts <- diff(germany_ts)
d1germany_ts <- d1germany_ts[-1]
#plot(d1germany_ts)

#ARMA(4,0)-gjrGARCH(1,1) t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m_germany=ugarchfit(spec=spec2,data=d1germany_ts)
#m_germany
#plot(m_germany)

#Volatility series
volgermany = sigma(m_germany) 
volgermany_an=(252)^0.5*volgermany
#plot(volgermany_an)

#Volatility vs. CISS
#time = data.frame(germany_ts[-1], volgermany_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)



                                    ### France ###

d1france_ts <- diff(france_ts)
d1france_ts <- d1france_ts[-1]
#plot(d1france_ts)

#ARMA(4,0)-gjrGARCH(1,1) t-Student
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(4,0)), distribution.model = "std")
m_france=ugarchfit(spec=spec2,data=d1france_ts)
#m_france
#plot(m_france)

#Volatility series
volfrance = sigma(m_france) 
volfrance_an=(252)^0.5*volfrance
#plot(volfrance_an)

#Volatility vs. CISS
#time = data.frame(france_ts[-1], volfrance_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)




                                ### Italy ###

d1italy_ts <- diff(italy_ts)
d1italy_ts <- d1italy_ts[-1]
#plot(d1italy_ts)

#ARMA(1,1)-EGARCH(1,1) t-Student
spec3=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_italy=ugarchfit(spec=spec3,data=d1italy_ts)
#m_italy
#plot(m_italy)

#Volatility series
volitaly = sigma(m_italy) 
volitaly_an=(252)^0.5*volitaly
#plot(volitaly_an)

#Volatility vs. CISS
#time = data.frame(italy_ts[-1], volitaly_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)



                              ### Portugal ###

d1portugal_ts <- diff(portugal_ts)
d1portugal_ts <- d1portugal_ts[-1]
#plot(d1portugal_ts)

#ARMA(1,1)-gjrGARCH(1,1) t-Student 
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_portugal=ugarchfit(spec=spec2,data=d1portugal_ts)
#m_portugal
#plot(m_portugal)

#Volatility series
volportugal = sigma(m_portugal) 
volportugal_an=(252)^0.5*volportugal
#plot(volportugal_an)

#Volatility vs. CISS
#time = data.frame(portugal_ts[-1], volportugal_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)



                                     ### UK ###

d1uk_ts <- diff(uk_ts)
d1uk_ts <- d1uk_ts[-1]
#plot(d1uk_ts)

#ARMA(1,1)-GARCH(1,1) t-Student 
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)), distribution.model = "std")
m_uk=ugarchfit(spec=spec1,data=d1uk_ts)
#m_uk
#plot(m_uk)

#Volatility series
voluk = sigma(m_uk) 
voluk_an=(252)^0.5*voluk
#plot(voluk_an)

#Volatility vs. CISS
#time = data.frame(uk_ts[-1], voluk_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)


                                   ### Spain ###

d1spain_ts <- diff(spain_ts)
d1spain_ts <- d1spain_ts[-1]
#plot(d1spain_ts)

#ARMA(3,0)-gjrGARCH(1,1) t-Student 
spec2=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(3,0)), distribution.model = "std")
m_spain=ugarchfit(spec=spec2,data=d1spain_ts)
#m_spain
#plot(m_spain)

#Volatility series
volspain = sigma(m_spain) 
volspain_an=(252)^0.5*volspain
#plot(volspain_an)

#Volatility vs. CISS
#time = data.frame(spain_ts[-1], volspain_an)
#ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
#legend("topleft", c("CISS","Volatility"), lty=c(1,1), col=c("black","red"), cex=0.6)



########################
#   VAR(p) fitting     # Euro Area. Volatility series.
########################

length(volgermany_an)
length(volfrance_an)

CISS <- as.zoo(data.frame(volgermany_an,volfrance_an,volitaly_an,volspain_an,volportugal_an,voluk_an))
plot(CISS)

### VAR(p) ###

#lag selection
VARselect(CISS, lag.max =10)
p.lag <- 2

#estimation
var.mod <- VAR(CISS, p=p.lag)
class(var.mod)

#prediction
var.pred <-predict(var.mod, n.ahead = 100)
plot(var.pred)

#validation
summary(var.mod)
acf(residuals(var.mod))
acf(residuals(var.mod)[,1:3])
acf(residuals(var.mod)[,4:5])
serial.test(var.mod, type="BG")    #Test for serially correlated errors (H0: errors are not serially correlated)
normality.test(var.mod, multivariate.only = TRUE) #Residuals not normally distributted.

#Var(p) roots (eigenvalues should have modulus less than 1)
roots(var.mod)

#MA representation
H <- 12  #horizon
VMAcoef <- Phi(var.mod, nstep=H)


########################
#   Conectedness       #
########################

#install.packages("Spillover")

#Hyperparameters
H <- 10 #horizon
p.lag <- 2 #VAR(p)
window <- 200 #width rolling sample

#Generalized Forecast Error Variance Decomposition
class(var.mod)
gFEVD <- g.fevd(var.mod, n.ahead = H, normalized = TRUE)
gFEVD

#Generalized spillover index (Conectedness Table)
ConTable <- G.spillover(var.mod, n.ahead = H, standardized = FALSE)
class(ConTable)
ConTable
ConTable[1:4,1:4]   #FEVD H-step-ahead

#Net spillovers
net(ConTable)

#Dynamic pairwise directional conectedness ij

#dynTable <- array(dim=c(8,7,length(1:(dim(CISS)[1]-200+1))))
#dim(dynTable)

roll.pairwise = function(wind=200, ahead=10, Varp=2){
  
  dynTable <<- array(dim=c(8,7,length(1:(dim(CISS)[1]-wind+1))))
  
  for (k in 1:(dim(CISS)[1]-wind+1)) {
    var.mod <- VAR(CISS[k:(k+wind-1)], p=Varp)
    ConTable <- G.spillover(var.mod, n.ahead = ahead, standardized = FALSE)
    dynTable[,,k] <<- ConTable
  }
}

roll.pairwise()

ConTable
ts.plot(dynTable[3,5,]) # Dynamic pairwise directional connectedness i <- j (italy <- portugal)
ts.plot(dynTable[5,3,]) # Dynamic pairwise directional connectedness i <- j (portugal <- italy)
ts.plot(dynTable[5,3,]-dynTable[3,5,]) #Net pairwise directional connectedness ij (j<-i - i<-j) (italy-portugal)
abline(h = 0, lty = "dashed", col = "black")

#Dynamic (Net) Spillover Index (Estimates the dynamic spillover index given a moving window)
plot(CISS)
DSI_net <- roll.net(CISS, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI_net)

#Dynamic Spillover Index
DSI <- roll.spillover(CISS, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI)

#DSI vs. "mean" CISS (or vs. Euro CISS) (or vs. Euro vol)
CISSv2 <- data.frame(germany_ts,france_ts,italy_ts,spain_ts,portugal_ts,uk_ts)
CISSv2$row_mean <- rowMeans(CISSv2)
CISSv2 <- as.zoo(CISSv2)
plot(CISSv2$row_mean)

CISSv3 <- data.frame(volgermany_an,volfrance_an,volitaly_an,volspain_an,volportugal_an,voluk_an)
CISSv3$row_mean <- rowMeans(CISSv3)
CISSv3 <- as.zoo(CISSv3)
plot(CISSv3$row_mean)

length(DSI)
dim(CISSv2)

DSI[1]
CISSv2[201,]
dim(CISSv2[201:5755,])
length(DSI)
head(CISSv2[201:5755,])
head(DSI)
tail(CISSv2[201:5755,])
tail(DSI)

time = data.frame(CISSv2$row_mean[201:5755,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)

time = data.frame(euro_ts[201:5755,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)

time = data.frame(voleuro_an[200:5754,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)

time = data.frame(CISSv3$row_mean[200:5754,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)



########################
#   VAR(p) fitting     # Euro Area. Differencied series.
########################

length(germany_ts)
length(france_ts)
length(italy_ts)
length(spain_ts)
length(portugal_ts)
length(uk_ts)

# Work with differencied series
d1germany <- diff(germany_ts)
d1germany <- d1germany[-1]

d1france <- diff(france_ts)
d1france <- d1france[-1]

d1italy <- diff(italy_ts)
d1italy <- d1italy[-1]

d1spain <- diff(spain_ts)
d1spain <- d1spain[-1]

d1portugal <- diff(portugal_ts)
d1portugal <- d1portugal[-1]

d1uk <- diff(uk_ts)
d1uk <- d1uk[-1]

CISS <- as.zoo(data.frame(d1germany,d1france,d1italy,d1spain,d1portugal,d1uk))
plot(CISS)

### VAR(p) ###

#lag selection
VARselect(CISS, lag.max =10)
p.lag <- 2

#estimation
var.mod <- VAR(CISS, p=p.lag)
class(var.mod)

#prediction
var.pred <-predict(var.mod, n.ahead = 100)
plot(var.pred)

#validation
summary(var.mod)
acf(residuals(var.mod))
acf(residuals(var.mod)[,1:3])
acf(residuals(var.mod)[,4:5])
serial.test(var.mod, type="BG")    #Test for serially correlated errors (H0: errors are not serially correlated)
normality.test(var.mod, multivariate.only = TRUE) #Residuals not normally distributted.

#Var(p) roots (eigenvalues should have modulus less than 1)
roots(var.mod)

#MA representation
H <- 12  #horizon
VMAcoef <- Phi(var.mod, nstep=H)


########################
#   Conectedness       #
########################

#install.packages("Spillover")

#Hyperparameters
H <- 10 #horizon
p.lag <- 2 #VAR(p)
window <- 200 #width rolling sample

#Generalized Forecast Error Variance Decomposition
class(var.mod)
gFEVD <- g.fevd(var.mod, n.ahead = H, normalized = TRUE)
gFEVD

#Generalized spillover index (Conectedness Table)
ConTable <- G.spillover(var.mod, n.ahead = H, standardized = FALSE)
class(ConTable)
ConTable
ConTable[1:4,1:4]   #FEVD H-step-ahead

#Net spillovers
net(ConTable)

#Dynamic (Net) Spillover Index (Estimates the dynamic spillover index given a moving window)
plot(CISS)
DSI_net <- roll.net(CISS, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI_net)

#Dynamic Spillover Index
DSI <- roll.spillover(CISS, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI)

#DSI vs. "mean" CISS (or vs. Euro CISS)
CISSv2 <- data.frame(germany_ts,france_ts,italy_ts,spain_ts,portugal_ts,uk_ts)
CISSv2$row_mean <- rowMeans(CISSv2)
CISSv2 <- as.zoo(CISSv2)
plot(CISSv2$row_mean)

length(DSI)
dim(CISSv2)

DSI[1]
CISSv2[201,]
dim(CISSv2[201:5755,])
length(DSI)
head(CISSv2[201:5755,])
head(DSI)
tail(CISSv2[201:5755,])
tail(DSI)

time = data.frame(CISSv2$row_mean[201:5755,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)

time = data.frame(euro_ts[201:5755,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)



########################
#   VAR(p) fitting     #  ON Series ! #  For Euro, US and China
########################

join1 <- full_join(us,euro,by="date")
sum(is.na(join1))
join2 <- full_join(join1, china, by="date")
colnames(join2) <- c("date", "us", "euro", "china")
sum(is.na(join2$china))
join2 <- join2 %>% fill(china, .direction = "up")
sum(is.na(join2$china))

ts <- xts(join2[2:4], join2$date)
plot(ts)

ts <- ts[1593:dim(ts)[1]]
plot(ts)

head(ts)
tail(ts)
sum(is.na(ts))

CISS <- ts

#lag selection
VARselect(CISS, lag.max = 5)
p.lag <- 5

#estimation
var.mod <- VAR(CISS, p=p.lag)
class(var.mod)

#prediction
var.pred <-predict(var.mod, n.ahead = 100)
plot(var.pred)

#validation
summary(var.mod)
acf(residuals(var.mod))
serial.test(var.mod, type="BG")    #Test for serially correlated errors (H0: errors are not serially correlated)
normality.test(var.mod, multivariate.only = TRUE) #Residuals not normally distributted.

#Var(p) roots (eigenvalues should have modulus less than 1)
roots(var.mod)

#MA representation
H <- 12  #horizon
VMAcoef <- Phi(var.mod, nstep=H)

########################
#   Conectedness       #
########################

#install.packages("Spillover")

#Hyperparameters
H <- 10 #horizon
p.lag <- 5 #VAR(p)
window <- 200 #width rolling sample

#Generalized Forecast Error Variance Decomposition
class(var.mod)
gFEVD <- g.fevd(var.mod, n.ahead = H, normalized = TRUE)
gFEVD

#Generalized spillover index (Conectedness Table)
ConTable <- G.spillover(var.mod, n.ahead = H, standardized = FALSE)
class(ConTable)
ConTable
ConTable[1:4,1:4]   #FEVD H-step-ahead

#Net spillovers
net(ConTable)

#Dynamic pairwise directional conectedness ij

dim(ConTable)
roll.pairwise = function(wind=200, ahead=10, Varp=5){
  
  dynTable <<- array(dim=c(5,4,length(1:(dim(CISS)[1]-wind+1))))
  
  for (k in 1:(dim(CISS)[1]-wind+1)) {
    var.mod <- VAR(CISS[k:(k+wind-1)], p=Varp)
    ConTable <- G.spillover(var.mod, n.ahead = ahead, standardized = FALSE)
    dynTable[,,k] <<- ConTable
  }
}

roll.pairwise()

ConTable
ts.plot(dynTable[1,2,]) # Dynamic pairwise directional connectedness i <- j (us <- euro)
ts.plot(dynTable[2,1,]) # Dynamic pairwise directional connectedness i <- j (euro <- us)
ts.plot(dynTable[2,1,]-dynTable[1,2,]) #Net pairwise directional connectedness ij (j<-i - i<-j) (us-euro)
abline(h = 0, lty = "dashed", col = "black")

#Dynamic (Net) Spillover Index (Estimates the dynamic spillover index given a moving window)
plot(CISS)
CISS <- as.zoo(CISS)
DSI_net <- roll.net(CISS, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI_net)

#Dynamic Spillover Index
DSI <- roll.spillover(CISS, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI)

#DSI vs. "mean" CISS (or vs. Euro CISS) (or vs. Euro vol)

DSI[1]
head(euro_ts[1792:5755,])

time = data.frame(euro_ts[1792:5755,], DSI/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)


                              



















