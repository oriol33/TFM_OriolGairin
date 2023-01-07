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
library(viridis) 


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


### Time series Dates ###
dates <- euro$date
dates_china <- china$date


##################################
#   Nice Plots & descriptive     # 3.2 CISS Overview
##################################

#Graph formating

#windowsFonts()
#library(extrafont)
#library(gridExtra)
#loadfonts(device = "win")
#font_import()
#y
#par(family = "Times New Roman")

#ibrary(RColorBrewer)
#rewer.pal(n = 6, name = "Dark2")
#rewer.pal(n = 6, name = "Spectral")

#Simple graphs
CISSplots_euro <- data.frame(germany_ts,france_ts,italy_ts,spain_ts,portugal_ts,uk_ts)
ts.plot(CISSplots_euro, gpars= list(xlab="time", ylab="CISS", col = 1:ncol(CISSplots_euro)), ylim=c(0,1))
legend("topleft", c("Germany","France","Italy","Spain","Portugal","UK"), lty=c(1), col=1:ncol(CISSplots_euro), cex=0.6)

join1 <- full_join(us,euro,by="date")
join2 <- full_join(join1, china, by="date")
colnames(join2) <- c("date", "us", "euro", "china")
join2 <- join2 %>% fill(china, .direction = "up")
ts <- xts(join2[2:4], join2$date)

CISSplots_world <- data.frame(ts)
ts.plot(CISSplots_world, gpars= list(xlab="time", ylab="CISS", col = 1:ncol(CISSplots_world)), ylim=c(0,1))
legend("topleft", c("US", "Europe", "China"), lty=c(1), col=1:ncol(CISSplots_world), cex=0.6)

### ggplot2
data <- CISSplots_euro
data$day <- rev(dates)

# Euro Area
p <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = germany_ts),color="#D53E4F", size = 1, alpha=0.7) + 
  geom_line(aes(y = france_ts), color="#FC8D59", size = 1, alpha=0.7) +
  geom_line(aes(y = spain_ts), color="#FEE08B", size = 1, alpha=0.7) +
  geom_line(aes(y = portugal_ts), color="#E6F598", size = 1, alpha=0.7) +
  geom_line(aes(y = italy_ts), color="#99D594", size = 1, alpha=0.7) +
  geom_line(aes(y = uk_ts), color="#3288BD", size = 1, alpha=0.7) +
  labs(x = "Time", y = "CISS", color = "Legend") + scale_color_manual(values = colors)+
  theme_light()
p

colors <- c("Germany" = "#D53E4F", "France" = "#FC8D59", "Spain"="#FEE08B", 
            "Portugal"= "#E6F598", "Italy"="#99D594", "UK"="#3288BD")

p <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = germany_ts, color="Germany"), size = 1) + 
  geom_line(aes(y = france_ts, color="France"), size = 1)+
  geom_line(aes(y = spain_ts, color="Spain"), size = 1)+
  geom_line(aes(y = portugal_ts, color="Portugal"), size = 1)+
  geom_line(aes(y = italy_ts, color="Italy"), size = 1)+
  geom_line(aes(y = uk_ts, color="UK"), size = 1)+
  labs(x = "Time", y = "CISS", color = "Region") +ylim(0,0.95)+
  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
p

# World
data <- CISSplots_world
data$day <- rev(dates)
p <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = euro),color="black") + 
  geom_line(aes(y = us), color="red") +
  geom_line(aes(y= china), color="blue")+
  xlab("Time")+ylab("CISS")+ylim(0,1)+
  theme_minimal()
p

colors <- c("Euro Area" = "#D53E4F", "US" = "#FEE08B", "China"="#3288BD")

p <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = euro, color="Euro Area"), size = 1) + 
  geom_line(aes(y = us, color="US"), size = 1)+
  geom_line(aes(y = china, color="China"), size = 1)+
  labs(x = "Time", y = "CISS", color = "Region", ylim=c(0,1)) +ylim(0,0.95)+
  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
p

#Euro CISS with anotations
data <- CISSplots_world
data$day <- rev(dates)
p <- ggplot(data, aes(x=day)) +
  geom_line(aes(y = euro),color="#3288BD", size=1) + 
  geom_vline(xintercept =as.Date("2001-09-11"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2002-11-01"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2003-03-19"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2007-08-01"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2008-03-01"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2008-09-20"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2010-05-10"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2011-08-01"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2016-06-14"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2020-03-12"), color="black", size=1, lty=2, alpha=0.6)+
  geom_vline(xintercept =as.Date("2022-02-24"), color="black", size=1, lty=2, alpha=0.6)+
  labs(x = "Time", y = "Euro Area CISS") +ylim(0,0.95)+
  theme(text = element_text(family = "serif", size=16))
p

max(CISSplots_world$euro)
which(CISSplots_world$euro == max(CISSplots_world$euro))
euro_ts[2145]

#### Descriptive stats

normalTest(china$value,method="jb") # Reject Normality
normalTest(euro$value,method="jb")
normalTest(germany$value,method="jb")

basicStats(CISSplots_world) 
basicStats(CISSplots_euro)
basicStats(china_ts)
head(euro_ts)
tail(euro_ts)
head(china_ts)
tail(china_ts)


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

CISS_euro <- as.zoo(data.frame(d1germany,d1france,d1italy,d1spain,d1portugal,d1uk))

### VAR(p) ###

#lag selection
VARselect(CISS_euro, lag.max =10)
p.lag <- 2

#estimation
var.mod_euro <- VAR(CISS_euro, p=p.lag)

########################
#   Conectedness       #
########################

#Hyperparameters
H <- 10 #horizon
p.lag <- 2 #VAR(p)
window <- 250 #width rolling sample

#Generalized spillover index (Conectedness Table)
ConTable_euro <- G.spillover(var.mod_euro, n.ahead = H, standardized = FALSE)
ConTable_euro[1:7,1:7]

#Net spillovers
net(ConTable_euro)

#Dynamic (Net) Spillover Index (Estimates the dynamic spillover index given a moving window)
DSI_net_euro <- roll.net(CISS_euro, width= window, n.ahead= H, index= "generalized", p=p.lag)
colnames(DSI_net_euro) <- c("Germany", "France", "Italy", "Spain", "Portugal", "UK")
plot(DSI_net_euro, xlab="Time", main="Net Total Directional Connectedness", ylim=c(-6,6))

#Graphs formating 

#data <- as.data.frame(DSI_net_euro)
#dim(data)
#dates2 <- rev(dates)[(length(dates)-5505+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#p1 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,1]), size = 1, color="#3288BD") + 
#  labs(x = "", y = "Germany") +ylim(-5.8,5.8)+
#  theme(text = element_text(family = "serif", size=16))
#p1
#
#p2 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,2]), size = 1, color="#3288BD") + 
#  labs(x = "", y = "France") +ylim(-5.8,5.8)+
#  theme(text = element_text(family = "serif", size=16))
#p2
#
#p3 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,3]), size = 1, color="#3288BD") + 
#  labs(x = "", y = "Italy") +ylim(-5.8,5.8)+
#  theme(text = element_text(family = "serif", size=16))
#p3
#
#p4 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,4]), size = 1, color="#3288BD") + 
#  labs(x = "", y = "Spain") +ylim(-5.8,5.8)+
#  theme(text = element_text(family = "serif", size=16))
#p4
#
#p5 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,5]), size = 1, color="#3288BD") + 
#  labs(x = "Time", y = "Portugal") +ylim(-5.8,5.8)+
#  theme(text = element_text(family = "serif", size=16))
#p5
#
#p6 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,6]), size = 1, color="#3288BD") + 
#  labs(x = "Time", y = "UK") +ylim(-5.8,5.8)+
#  theme(text = element_text(family = "serif", size=16))
#p6
#
#grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)

#Dynamic Spillover Index
DSI_euro <- roll.spillover(CISS_euro, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI_euro, xlab="Time", ylab="Total connectedness")
max(DSI_euro)
min(DSI_euro)
which(DSI_euro == min(DSI_euro))
DSI_euro[4842]
DSI_euro[3366]

#DSI vs. "mean" CISS (or vs. Euro CISS)
time = data.frame(euro_ts[251:5755,], DSI_euro/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)

#Graphs formating

#data <- time
#dim(data)
#dates2 <- rev(dates)[(length(dates)-5505+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#colors <- c("Euro Area CISS" = "#3288BD", "Total Connectedness" = "#D53E4F")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_line(aes(y = data[,1], color="Euro Area CISS"), size = 1) + 
#  geom_line(aes(y = data[,2], color="Total Connectedness"), size = 1)+
#  labs(x = "Time", y = "", color = "Series", ylim=c(0,1)) +ylim(0,0.95)+
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p

#Dynamic pairwise directional conectedness ij
ConTable_euro[1:7,1:7]
#Germany & france maximum exchange
#Spain & france 2nd maximum exchange
roll.pairwise = function(wind=250, ahead=10, Varp=2){
  
  dynTable <<- array(dim=c(8,7,length(1:(dim(CISS_euro)[1]-wind+1))))
  
  for (k in 1:(dim(CISS_euro)[1]-wind+1)) {
    var.mod <- VAR(CISS_euro[k:(k+wind-1)], p=Varp)
    ConTable <- G.spillover(var.mod, n.ahead = ahead, standardized = FALSE)
    dynTable[,,k] <<- ConTable
  }
}

roll.pairwise(wind=250, ahead=10, Varp=2)

ts.plot(dynTable[1,2,], ylim=c(-8,25), ylab="Germany-France") # Dynamic pairwise directional connectedness i <- j (germany <- france)
lines(dynTable[2,1,],col="red") # Dynamic pairwise directional connectedness i <- j (france <- germany)
lines(dynTable[2,1,]-dynTable[1,2,], col="blue") #Net pairwise directional connectedness ij (j<-i - i<-j) (germany-france)
abline(h = 0, lty = "dashed", col = "black")
dynTable[1,2,][4100]
DSI_euro[4100]

ts.plot(dynTable[4,2,], ylim=c(-6,35), ylab="Spain-France") # Dynamic pairwise directional connectedness i <- j (spain <- france)
lines(dynTable[2,4,], col="red") # Dynamic pairwise directional connectedness i <- j (france <- spain)
lines(dynTable[2,4,]-dynTable[4,2,], col="blue") #Net pairwise directional connectedness ij (j<-i - i<-j) (spain-france)
abline(h = 0, lty = "dashed", col = "black")
max(dynTable[4,2,])

ts.plot(dynTable[2,2,])#france-france


#Graphs formating

#data <- as.data.frame(dynTable[1,2,])
#data2 <- as.data.frame(dynTable[2,1,])
#data3 <- as.data.frame(dynTable[2,1,]-dynTable[1,2,])
#data$sec <- as.data.frame(dynTable[2,1,])
#data$third <- as.data.frame(dynTable[2,1,]-dynTable[1,2,])
#dim(data)
#dates2 <- rev(dates)[(length(dates)-5505+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#attributes(data)
#
#colnames(data) <- c("one", "two", "three", "day")
#ts(data)
#
#
#colors <- c("France to Germany" = "#D53E4F", "Germany to France" = "#FEE08B", "Net"="#3288BD")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,1], color="France to Germany"), size = 1) +
#  geom_line(aes(y = data2[,1], color="Germany to France"), size = 1)+
#  geom_line(aes(y = data3[,1], color="Net"), size = 1) +ylim(-8,35)+
#  labs(x = "", y = "Germany-France", color = "Pairwise C.") +
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p

#data4 <- as.data.frame(dynTable[4,2,])
#data5 <- as.data.frame(dynTable[2,4,])
#data6 <- as.data.frame(dynTable[2,4,]-dynTable[4,2,])
#dim(data)
#dates2 <- rev(dates)[(length(dates)-5505+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#attributes(data)
#
#colnames(data) <- c("one", "two", "three", "day")
#ts(data)
#
#
#colors2 <- c("France to Spain" = "#D53E4F", "Spain to France" = "#FEE08B", "Net"="#3288BD")
#
#p2 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data4[,1], color="France to Spain"), size = 1) +
#  geom_line(aes(y = data5[,1], color="Spain to France"), size = 1)+
#  geom_line(aes(y = data6[,1], color="Net"), size = 1) +ylim(-8,35)+
#  labs(x = "Time", y = "Spain-France", color = "Pairwise C.") +
#  scale_color_manual(values = colors2)+theme(text = element_text(family = "serif", size=16))
#p2
#
#
#grid.arrange(p, p2, nrow = 2)




### Robustness analysis ###
## H

roll.spillover
G.spillover

#G.spillover_mod  (MODIFIED FUN)
G.spillover_mod <- function (x, n.ahead=H, standardized = TRUE) 
{
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
  }
  var.est <- x
  n.ahead <- abs(as.integer(n.ahead))
  EVD <- g.fevd(var.est, n.ahead = n.ahead)
  K <- var.est$K
  table <- do.call(rbind, lapply(EVD, "[", n.ahead, )) * 
    100
  CFO <- rowSums(table - diag(diag(table)))
  CTO <- colSums(table) - diag(table)
  CTOT <- colSums(table)
  table <- rbind(table, `C. to others (spillover)` = CTO, 
                 `C. to others including own` = CTOT)
  index <- function(x) {
    (sum(x) - sum(diag(x)))/nrow(x)
  }
  if (standardized) {
    table <- cbind(table, `C. from others` = c(CFO, 
                                               sum(CFO), sum(CTOT)))/K
  }
  else {
    table <- cbind(table, `C. from others` = c(CFO, 
                                               index(table[1:K, ]), sum(CTOT)))
  }
  return(table)
}

#roll.spillover_mod (MODIFIED FUN)
roll.spillover_mod <- function (data, width, n.ahead = H, index = c("orthogonalized", 
                                 "generalized"), ortho.type = c("single", "partial", 
                                 "total"), ...) 
{
  if (!(class(data) == "zoo")) {
    stop("\nPlease provide an object of class 'zoo', generated by 'as.zoo'.\n")
  }
  K <- ncol(data) + 1
  roll.index <- switch(match.arg(index), orthogonalized = {
    switch(match.arg(ortho.type), single = {
      rollapply(data, width = width, FUN = function(z) {
        O.spillover(VAR(z, ...), ortho.type = c("single"))[K, 
                                                           K]
      }, by.column = FALSE, align = "right")
    }, partial = {
      rollapply(data, width = width, FUN = function(z) {
        O.spillover(VAR(z, ...), ortho.type = c("partial"))[K, 
                                                            K]
      }, by.column = FALSE, align = "right")
    }, total = {
      rollapply(data, width = width, FUN = function(z) {
        O.spillover(VAR(z, ...), ortho.type = c("total"))[K, 
                                                          K]
      }, by.column = FALSE, align = "right")
    })
  }, generalized = {
    rollapply(data, width = width, FUN = function(z) {
      G.spillover_mod(VAR(z, ...))[K, K]
    }, by.column = FALSE, align = "right")
  })
  return(roll.index)
}

########

H <- 10
DSI_H10_euro <- roll.spillover_mod(CISS_euro, width= 250, n.ahead=H, index="generalized", p=p.lag)
H <- 3
DSI_H3_euro <- roll.spillover_mod(CISS_euro, width= 250, n.ahead=H, index="generalized", p=p.lag)
H <- 15
DSI_H15_euro <- roll.spillover_mod(CISS_euro, width= 250, n.ahead=H, index="generalized", p=p.lag)

time2 = data.frame(DSI_H3_euro,DSI_H10_euro,DSI_H15_euro)
ts.plot(time2, gpars= list(xlab="time", ylab=",", col = 1:ncol(time2)))
legend("topleft", c("H=3","H=10", "H=15"), lty=c(1,1,1), col=1:ncol(time2), cex=0.6)

#Graphs formating

#data <- time2
#dates2 <- rev(dates)[(length(dates)-length(DSI_H3_euro)+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2

#colors <- c("H=3" = "#D53E4F", "H=10" = "#FEE08B", "H=15"="#3288BD")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_line(aes(y = DSI_H3_euro, color="H=3"), size = 1) +
#  geom_line(aes(y = DSI_H10_euro, color="H=10"), size = 1)+
#  geom_line(aes(y = DSI_H15_euro, color="H=15"), size = 1)+
#  labs(x = "Time", y = "Total Connectedness Index", color = "Horizon") +
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p

########

## Window
DSI_150_euro <- roll.spillover(CISS_euro, width= 150, n.ahead= 10, index= "generalized", p=p.lag)
DSI_200_euro <- roll.spillover(CISS_euro, width= 200, n.ahead= 10, index= "generalized", p=p.lag)
DSI_250_euro <- roll.spillover(CISS_euro, width= 250, n.ahead= 10, index= "generalized", p=p.lag)
DSI_300_euro <- roll.spillover(CISS_euro, width= 300, n.ahead= 10, index= "generalized", p=p.lag)
length(DSI_150_euro[101:length(DSI_150_euro)])
length(DSI_200_euro)
length(DSI_250_euro)

time = data.frame(DSI_150_euro[101:length(DSI_150_euro)], DSI_200_euro[51:length(DSI_200_euro)], DSI_250_euro)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("w=150","w=200","w=250"), lty=c(1,1,1), col=1:ncol(time), cex=0.6)

time = data.frame(DSI_200_euro[101:length(DSI_200_euro)], DSI_250_euro[51:length(DSI_250_euro)], DSI_300_euro)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("w=200","w=250","w=300"), lty=c(1,1,1), col=1:ncol(time), cex=0.6)

#Graphs formating

#data <- time
#dim(time)
#dates2 <- rev(dates)[(length(dates)-5455+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#colors <- c("w=200" = "#D53E4F", "w=250" = "#FEE08B", "w=300"="#3288BD")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_line(aes(y = data[,1], color="w=200"), size = 1) +
#  geom_line(aes(y = data[,2], color="w=250"), size = 1)+
#  geom_line(aes(y = data[,3], color="w=300"), size = 1)+
#  labs(x = "Time", y = "Total Connectedness Index", color = "Window width") +
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p



########################
#   VAR(p) fitting     #  Euro, US, China. Differencied series
########################

join1 <- full_join(us,euro,by="date")
sum(is.na(join1))
join2 <- full_join(join1, china, by="date")
colnames(join2) <- c("date", "us", "euro", "china")
sum(is.na(join2$china))
join2 <- join2 %>% fill(china, .direction = "up")
sum(is.na(join2$china))

ts <- xts(join2[2:4], join2$date)

ts <- ts[1593:dim(ts)[1]]

head(ts)
tail(ts)
sum(is.na(ts))

tsdiff <- diff(ts)
tsdiff <- tsdiff[-1,]
plot(tsdiff)

CISS_world <- tsdiff

#lag selection
VARselect(CISS_world, lag.max = 10)
p.lag <- 4

#estimation
var.mod_world <- VAR(CISS_world, p=p.lag)

########################
#   Conectedness       #
########################

#Hyperparameters
H <- 10 #horizon
p.lag <- 4 #VAR(p)
window <- 250 #width rolling sample

#Generalized spillover index (Conectedness Table)
ConTable_world <- G.spillover(var.mod_world, n.ahead = H, standardized = FALSE)
ConTable_world[1:4,1:4] 

#Net spillovers
net(ConTable_world)

#Dynamic pairwise directional conectedness ij

roll.pairwise2 = function(wind=250, ahead=10, Varp=4){
  
  dynTable2 <<- array(dim=c(5,4,length(1:(dim(CISS_world)[1]-wind+1))))
  
  for (k in 1:(dim(CISS_world)[1]-wind+1)) {
    var.mod <- VAR(CISS_world[k:(k+wind-1)], p=Varp)
    ConTable <- G.spillover(var.mod, n.ahead = ahead, standardized = FALSE)
    dynTable2[,,k] <<- ConTable
  }
}

roll.pairwise2(wind=250, ahead=10, Varp=4)

ts.plot(dynTable2[1,2,], ylab="US-Euro", ylim=c(-10,50)) # Dynamic pairwise directional connectedness i <- j (us <- euro)
lines(dynTable2[2,1,], col="red") # Dynamic pairwise directional connectedness i <- j (euro <- us)
lines(dynTable2[2,1,]-dynTable2[1,2,], col="blue") #Net pairwise directional connectedness ij (j<-i - i<-j) (us-euro)
abline(h = 0, lty = "dashed", col = "black")

ts.plot(dynTable2[3,2,], ylab="China-Euro", ylim=c(-20,30)) # Dynamic pairwise directional connectedness i <- j (china <- euro)
lines(dynTable2[2,3,], col="red") # Dynamic pairwise directional connectedness i <- j (euro <- china)
lines(dynTable2[2,3,]-dynTable2[3,2,], col="blue") #Net pairwise directional connectedness ij (j<-i - i<-j) (china-euro)
abline(h = 0, lty = "dashed", col = "black")

#Graphs formating

#data <- as.data.frame(dynTable2[1,2,])
#data2 <- as.data.frame(dynTable2[2,1,])
#data3 <- as.data.frame(dynTable2[2,1,]-dynTable2[1,2,])
#
#dim(data)
#dates2 <- rev(dates)[(length(dates)-3913+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#attributes(data)
#
#colnames(data) <- c("one", "two", "three", "day")
#ts(data)
#
#
#colors <- c("Euro Area to US" = "#D53E4F", "US to Euro Area" = "#FEE08B", "Net"="#3288BD")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,1], color="Euro Area to US"), size = 1) +
#  geom_line(aes(y = data2[,1], color="US to Euro Area"), size = 1)+
#  geom_line(aes(y = data3[,1], color="Net"), size = 1) +ylim(-8,35)+
#  labs(x = "Time", y = "US-Euro Area", color = "Pairwise C.") +
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p



##Dynamic (Net) Spillover Index (Estimates the dynamic spillover index given a moving window)
CISS_world <- as.zoo(CISS_world)
DSI_net_world <- roll.net(CISS_world, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI_net_world)

#Graphs formating

#data <- as.data.frame(DSI_net_world)
#dim(data)
#dates2 <- rev(dates)[(length(dates)-3913+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#p1 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,1]), size = 1, color="#3288BD") + 
#  labs(x = "", y = "US") +ylim(-10,15)+
#  theme(text = element_text(family = "serif", size=16))
#p1
#
#p2 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,2]), size = 1, color="#3288BD") + 
#  labs(x = "", y = "Euro Area") +ylim(-10,15)+
#  theme(text = element_text(family = "serif", size=16))
#p2
#
#p3 <- ggplot(data, aes(x=day)) +
#  geom_hline(yintercept=0, size=1, linetype="dashed", alpha=0.6)+
#  geom_line(aes(y = data[,3]), size = 1, color="#3288BD") + 
#  labs(x = "Time", y = "China") +ylim(-10,15)+
#  theme(text = element_text(family = "serif", size=16))
#p3
#
#grid.arrange(p1, p2, p3, nrow = 3)


#Dynamic Spillover Index
DSI_world <- roll.spillover(CISS_world, width= window, n.ahead= H, index= "generalized", p=p.lag)
plot(DSI_world)

#DSI vs. "mean" CISS
DSI_world[1]
head(euro_ts[1843:5755,])

time = data.frame(euro_ts[1843:5755,], DSI_world/100)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("CISS","DSI"), lty=c(1,1), col=c("black","red"), cex=0.6)

#Graphs formating

#data <- time
#dim(data)
#dates2 <- rev(dates)[(length(dates)-3913+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#colors <- c("Euro Area CISS" = "#3288BD", "Total Connectedness" = "#D53E4F")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_line(aes(y = data[,1], color="Euro Area CISS"), size = 1) + 
#  geom_line(aes(y = data[,2], color="Total Connectedness"), size = 1)+
#  labs(x = "Time", y = "", color = "Series", ylim=c(0,1)) +ylim(0,0.95)+
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p


### Robustness analysis ###
## H

H <- 10
DSI_H10_world <- roll.spillover_mod(CISS_world, width= 250, n.ahead=H, index="generalized", p=p.lag)
H <- 3
DSI_H3_world <- roll.spillover_mod(CISS_world, width= 250, n.ahead=H, index="generalized", p=p.lag)
H <- 15
DSI_H15_world <- roll.spillover_mod(CISS_world, width= 250, n.ahead=H, index="generalized", p=p.lag)

time2 = data.frame(DSI_H3_world,DSI_H10_world,DSI_H15_world)
ts.plot(time2, gpars= list(xlab="time", ylab=",", col = 1:ncol(time2)))
legend("topleft", c("H=3","H=10", "H=15"), lty=c(1,1,1), col=1:ncol(time2), cex=0.6)

#Graphs formating

#data <- time2
#dates2 <- rev(dates)[(length(dates)-length(DSI_H3_world)+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#colors <- c("H=3" = "#D53E4F", "H=10" = "#FEE08B", "H=15"="#3288BD")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_line(aes(y = DSI_H3_world, color="H=3"), size = 1) +
#  geom_line(aes(y = DSI_H10_world, color="H=10"), size = 1)+
#  geom_line(aes(y = DSI_H15_world, color="H=15"), size = 1)+
#  labs(x = "Time", y = "Total Connectedness Index", color = "Horizon") +
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p


## Window
DSI_150_world <- roll.spillover(CISS_world, width= 150, n.ahead= 10, index= "generalized", p=p.lag)
DSI_200_world <- roll.spillover(CISS_world, width= 200, n.ahead= 10, index= "generalized", p=p.lag)
DSI_250_world <- roll.spillover(CISS_world, width= 250, n.ahead= 10, index= "generalized", p=p.lag)
DSI_300_world <- roll.spillover(CISS_world, width= 300, n.ahead= 10, index= "generalized", p=p.lag)
length(DSI_150_world[101:length(DSI_150_world)])
length(DSI_200_world)
length(DSI_250_world)

time = data.frame(DSI_150_world[101:length(DSI_150_world)], DSI_200_world[51:length(DSI_200_world)], DSI_250_world)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("w=150","w=200","w=250"), lty=c(1,1,1), col=1:ncol(time), cex=0.6)

time = data.frame(DSI_200_world[101:length(DSI_200_world)], DSI_250_world[51:length(DSI_250_world)], DSI_300_world)
ts.plot(time, gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("w=200","w=250","w=300"), lty=c(1,1,1), col=1:ncol(time), cex=0.6)

#Graphs formating

#data <- time
#dim(time)
#dates2 <- rev(dates)[(length(dates)-3863+1):length(rev(dates))]
#length(dates2)
#dim(data)
#data$day <- dates2
#
#colors <- c("w=200" = "#D53E4F", "w=250" = "#FEE08B", "w=300"="#3288BD")
#
#p <- ggplot(data, aes(x=day)) +
#  geom_line(aes(y = data[,1], color="w=200"), size = 1) +
#  geom_line(aes(y = data[,2], color="w=250"), size = 1)+
#  geom_line(aes(y = data[,3], color="w=300"), size = 1)+
#  labs(x = "Time", y = "Total Connectedness Index", color = "Window width") +
#  scale_color_manual(values = colors)+theme(text = element_text(family = "serif", size=16))
#p


# Save whole workspace to working directory
save.image("20230107_conn.RData")

# Load workspace back to RStudio
#load("20221110_conn.RData")














