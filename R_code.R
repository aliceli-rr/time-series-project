rm(list=ls())
require(fGarch)

#1)
library(readxl)
CPE.data <- read_excel("C:/alice/Waterloo Stuff/STUDYMATERIALS/05. STAT443/Project/GDP_CONS_Canada.xls")
Wt <- as.numeric(as.character(CPE.data$GDP))
Wt2 <- as.numeric(as.character(CPE.data$CONS))
xlab_years <- seq(from=1961, to=2007, by= 0.25)
plot(xlab_years, Wt, xlab = "Time", ylab = "Wt", type = "l")
plot(xlab_years, Wt2, xlab = "Time", ylab = "W2t", type = "l")

#2)
#Convert into log form and plot
Xt <- log(Wt)

#TS
#linear regression of Xt
tSeq <- 1:length(Xt)
model1 <- lm(Xt ~ tSeq)
summary(model1)
#Yt is the residuals of regression
Yt <- model1$residuals
plot(xlab_years, Yt, xlab = "Time", ylab = "Yt", type = "l")


#DS
dX <- diff(Xt)
model2 <- lm(dX~1)
Yt2 <- model2$residuals
summary(model2)
xlab_years2 <- xlab_years[-1]
plot(xlab_years2, Yt2, xlab = "Time", ylab = "Yt",type = "l")

#3)
#function for BIC
BIC <- function(res, k, N){
  bic <- log(sum(res ^2) / N)
  bic <- bic + log(N) * k / N
}

#TS
#obtaining the bic
bic.array <- rep(NA, 9)
N <- length(Yt)
for (ii in 0:9){
  model.arima <- arima(Yt, order = c(ii, 0, 0))
  res.arima <- model.arima$residuals
  bic.array[ii + 1] <- BIC(res.arima, ii, N)
}

bic.array
which(bic.array ==  min(bic.array))
#the above result gives p = 2 is the best model for TS


#DS
bic.array2 <- rep(NA, 9)
N2 <- length(Yt2)
for (ii in 0:9){
  model.arima <- arima(Yt2, order = c(ii, 0, 0))
  res.arima <- model.arima$residuals
  bic.array2[ii + 1] <- BIC(res.arima, ii, N2)
}
bic.array2
which(bic.array2 ==  min(bic.array2))
#the above result gives p = 1 is the best model for DS


#Let's try AR(2) for TS
# Yt = Yt-1 +  Yt-2 
TSmodel.ar2 <- lm(Yt[-(1:2)] ~ Yt[-c(1, N)] + Yt[-c(N-1,N)] - 1)
summary(TSmodel.ar2)
TSsigma2.hat <- sum(TSmodel.ar2$residuals^2)/N
TSsigma2.hat
sd <- sqrt(TSsigma2.hat)
sd

#psi 
TSphi1 <- TSmodel.ar2$coefficients[1]
TSphi2 <- TSmodel.ar2$coefficients[2]

psi_array <- rep(NA, 185)
psi_array[1] <- 1
psi_array[2] <- TSphi1
for (ii in 3:185){
  psi_array[ii] <- TSphi1 * psi_array[ii-1] + TSphi2 * psi_array[ii-2]
}
psi_array[1:9]

#pho
pho_array <- rep(NA, 185)
pho_array[1] <- 1
pho_array[2] <- TSphi1/(1-TSphi2)
for (ii in 3:185){
  pho_array[ii] <- TSphi1 * pho_array[ii-1] + TSphi2 * pho_array[ii-2]
}
pho_array[1:9]

#sd
TSgamma <- TSsigma2.hat/(1-pho_array[2]*TSphi1 - pho_array[3]*TSphi2)
TSgamma
sqrt(TSgamma)

#4)
#DS
DSgrowthrate <- coefficients(model2)[["(Intercept)"]]
DSmodel.ar2 <- lm(Yt2[-(1:2)] ~ Yt2[-c(1, N2)] + Yt2[-c(N2-1,N2)] - 1)
summary(DSmodel.ar2)
#psi 
DSphi1 <- DSmodel.ar2$coefficients[1]
DSphi2 <- DSmodel.ar2$coefficients[2]
#Forecast of Yt array
DSForecast_Yt <- rep(NA, N2)
DSForecast_Yt[1] <- Yt2[N2]
DSForecast_Yt[2] <- DSphi1*Yt2[N2] + DSphi2 * Yt2[N2-1]
for (ii in 3:N2){
  DSForecast_Yt[ii] <- DSphi1 * DSForecast_Yt[ii-1] + DSphi2 * DSForecast_Yt[ii-2]
}
DSForecast_Yt[1:9]
DSForecast_DeltaXt<- DSgrowthrate + DSForecast_Yt
DSForecast_DeltaXt[1:9]

#Find psi for DS and then find Var of Forecast
DSsigma2.hat <- sum(DSmodel.ar2$residuals^2)/N2
DSsigma2.hat
DSpsi_array <- rep(NA, N2)
DSpsi_array[1] <- 1
DSpsi_array[2] <- DSphi1
for (ii in 3:N2){
  DSpsi_array[ii] <- DSphi1 * DSpsi_array[ii-1] + DSphi2 * DSpsi_array[ii-2]
}
DSpsi_array[1:9]


#pho
DSpho_array <- rep(NA, 185)
DSpho_array[1] <- 1
DSpho_array[2] <- DSphi1/(1-DSphi2)
for (ii in 3:185){
  DSpho_array[ii] <- DSphi1 * DSpho_array[ii-1] + DSphi2 * DSpho_array[ii-2]
}
DSpho_array[1:9]

DSForecast_VarXt <- rep(NA, N2)
DSForecast_VarXt[1] <- 0 
DSForecast_VarXt[2] <- DSsigma2.hat
for (ii in 3:N2){
  DSForecast_VarXt[ii] <-  
    DSsigma2.hat * sum((DSpsi_array[1:ii-1])^2)
}
DSForecast_VarXt[1:9]

#plotting
lwd_DSForecast <- DSForecast_DeltaXt-1.96*sqrt(DSForecast_VarXt)
upr_DSForecast <- DSForecast_DeltaXt+1.96*sqrt(DSForecast_VarXt)
lwd_DSForecast[1:9]
upr_DSForecast[1:9]

DSmatrix <- cbind(DSForecast_Yt[1:9], DSForecast_DeltaXt[1:9], DSForecast_VarXt[1:9], DSForecast_VarXt[1:9] , 
                  lwd_DSForecast[1:9], upr_DSForecast[1:9])
DSmatrix

yLim <- range(lwd_DSForecast[1:9],upr_DSForecast[1:9])
plot(DSForecast_DeltaXt[1:9], type="l", ylim = yLim, ylab ="Forcasted Growth Rate", xlab = "Lag k")
par(new=TRUE)
plot(lwd_DSForecast[1:9], type="l", ylab ="", ylim = yLim, xlab = "", col = "blue")
par(new=TRUE)
plot(upr_DSForecast[1:9], type="l", ylab="",  ylim = yLim, xlab = "", col = "blue")

#sub
yLim <- range(lwd_DSForecast[1:9],upr_DSForecast[1:9])
plot(DSForecast_DeltaXt[1:9], type="l", ylim = yLim, ylab ="Forecasted Delta Xt")
par(new=TRUE)
plot(lwd_DSForecast[1:9], type="l", ylab ="", ylim = yLim, col = "blue")
par(new=TRUE)
plot(upr_DSForecast[1:9], type="l", ylab="",  ylim = yLim, col = "red")


#TS
TSgrowthrate <- coefficients(model1)[2]
TSForecast_Yt <- rep(NA, 185)
TSForecast_Yt[1] <- Yt[N]
TSForecast_Yt[2] <- TSphi1*Yt[N] + TSphi2 * Yt[N-1]
for (ii in 3:185){
  TSForecast_Yt[ii] <- TSphi1 * TSForecast_Yt[ii-1] + TSphi2 * TSForecast_Yt[ii-2]
}
Yt[N-1]
Yt[N]
TSForecast_Yt[1:9]

TSForecast_DeltaXt <- rep(NA, 185)
TSForecast_DeltaXt[1] <- TSgrowthrate + TSForecast_Yt[1] - Yt[N-1] 
for (ii in 2:185){
  TSForecast_DeltaXt[ii] <- TSgrowthrate +  TSForecast_Yt[ii] - TSForecast_Yt[ii-1] 
}
TSForecast_DeltaXt[1:9]

TSForecast_VarXt <- rep(NA, 185)
TSForecast_VarXt[1] <- 0  #X0
TSForecast_VarXt[2] <- TSsigma2.hat #X1

for (k in 3:185){
  sum<-0
  for  (ii in 2:(k-1)) {
    sum <- sum + (psi_array[ii] - psi_array[ii-1])^2
  }
  TSForecast_VarXt[k] <- TSsigma2.hat * (1+sum)
}

TSForecast_VarXt[1:9]
sqrt(TSForecast_VarXt[1:9])
TSForecast_DeltaXt[1:9]-1.96*sqrt(TSForecast_VarXt[1:9])
TSForecast_DeltaXt[1:9]+1.96*sqrt(TSForecast_VarXt[1:9])

TSForecast_VarYt <- rep(NA, N)
TSForecast_VarYt[1] <- 0 
TSForecast_VarYt[2] <- TSsigma2.hat
for (ii in 3:N){
  TSForecast_VarYt[ii] <- TSsigma2.hat * sum((psi_array[1:ii-1])^2)
}

TSForecast_VarXt[1:9]
lwd_TSForecast <- TSForecast_DeltaXt-1.96*sqrt(TSForecast_VarXt)
upr_TSForecast <- TSForecast_DeltaXt+1.96*sqrt(TSForecast_VarXt)
TSmatrix <- cbind(TSForecast_Yt[1:9], TSForecast_DeltaXt[1:9], TSForecast_VarYt[1:9], TSForecast_VarXt[1:9] , 
                  lwd_TSForecast[1:9], upr_TSForecast[1:9])
TSmatrix

#plotting

yLim <- range(lwd_TSForecast,upr_TSForecast)
plot(TSForecast_DeltaXt, type="l", ylim = yLim, ylab ="")
par(new=TRUE)
plot(lwd_TSForecast, type="l", ylab ="", ylim = yLim, col = "blue")
par(new=TRUE)
plot(upr_TSForecast, type="l", ylab="",  ylim = yLim, col = "red")

#sub
yLim <- range(lwd_TSForecast[1:9],upr_TSForecast[1:9])
plot(TSForecast_DeltaXt[1:9], type="l", ylim = yLim, ylab ="Forecasted Growth Rate", xlab = "Lag k")
par(new=TRUE)
plot(lwd_TSForecast[1:9], type="l", ylab ="", ylim = yLim,xlab = "",  col = "blue")
par(new=TRUE)
plot(upr_TSForecast[1:9], type="l", ylab="",  ylim = yLim, xlab = "", col = "blue")

###################
#5)
require(tseries)
adf.test(Xt,k=5)
#6)
######TS##########
#Box-Jenkins
acf(Yt)$acf
2/sqrt(length(Yt))
#all rho(K) sig, therefore AR(q)
TSmodel.ar1 <- lm(Yt[-1] ~ Yt[-length(Yt)] - 1)
phi_11 <- as.numeric(TSmodel.ar1$coefficients)[1]
phi_11

phi_22 <- as.numeric(TSmodel.ar2$coefficients)[2]
phi_22

TSmodel.ar3 <- lm(Yt[-(1:3)] ~ Yt[-c(1:2, N)] + Yt[-c(1, (N-1):N)] + Yt[-c((N-2):N)] - 1)
phi_33 <- as.numeric(TSmodel.ar3$coefficients)[3]
phi_33

TSmodel.ar4 <- lm(Yt[-(1:4)] ~ Yt[-c(1:3, N)] + Yt[-c(1:2, N-1,N)]
                + Yt[-c(1, (N-2):N)] + Yt[-c((N-3):N)] - 1)
phi_44 <- as.numeric(TSmodel.ar4$coefficients)[4]
phi_44

TSmodel.ar5 <- lm(Yt[-(1:5)] ~ Yt[-c(1:4, N)] + Yt[-c(1:3, (N-1): N)]
                  + Yt[-c(1:2, (N-2):N)] + + Yt[-c(1, (N-3):N)] + Yt[-c((N-4):N)] - 1)
phi_55 <- as.numeric(TSmodel.ar5$coefficients)[5]
phi_55

TSmodel.ar6 <- lm(Yt[-(1:6)] ~ Yt[-c(1:5, N)] + Yt[-c(1:4, N-1,N)]
                  + Yt[-c(1:3, (N-2):N)] + Yt[-c(1:2, (N-3):N)]
                  + Yt[-c(1, (N-4):N)] + Yt[-c((N-5):N)] - 1)
phi_66 <- as.numeric(TSmodel.ar6$coefficients)[6]
phi_66

TSmodel.ar7 <- lm(Yt[-(1:7)] ~ Yt[-c(1:6, N)] + Yt[-c(1:5, N-1,N)]
                  + Yt[-c(1:4, (N-2):N)] + Yt[-c(1:3, (N-3):N)]
                  + Yt[-c(1:2, (N-4):N)] + Yt[-c(1, (N-5):N)] + Yt[-c((N-6):N)]- 1)
phi_77 <- as.numeric(TSmodel.ar7$coefficients)[7]
phi_77


TSmodel.ar8 <- lm(Yt[-(1:8)] ~ Yt[-c(1:7, N)] + Yt[-c(1:6, N-1,N)]
                  + Yt[-c(1:5, (N-2):N)] + Yt[-c(1:4, (N-3):N)]
                  + Yt[-c(1:3, (N-4):N)] + Yt[-c(1:2, (N-5):N)] +  Yt[-c(1, (N-6):N)] +  Yt[-c((N-7):N)]- 1)
phi_88 <- as.numeric(TSmodel.ar8$coefficients)[8]
phi_88


TSmodel.ar9 <- lm(Yt[-(1:9)] ~ Yt[-c(1:8, N)] + Yt[-c(1:7, N-1,N)]
                  + Yt[-c(1:6, (N-2):N)] + Yt[-c(1:5, (N-3):N)]
                  + Yt[-c(1:4, (N-4):N)] + Yt[-c(1:3, (N-5):N)] 
                  + Yt[-c(1:2, (N-6):N)] + Yt[-c(1, (N-7):N)] +  Yt[-c((N-8):N)]- 1)
phi_99 <- as.numeric(TSmodel.ar9$coefficients)[9]
phi_99

acf(Yt)$acf[1:10]
c(phi_11, phi_22, phi_33, phi_44, phi_55, phi_66, phi_77, phi_88, phi_99)

#choose AR(2)
TSstdres <- TSmodel.ar2$residuals/(sqrt(TSsigma2.hat))
plot(TSstdres, ylab="Standardized Residuals", 
     xlab="Time", 
     main="TS AR2", ylim = c(-4.2,4.2))
abline(h=-2, col="blue")
abline(h=2, col="blue")
abline(h=-3, col="red")
abline(h=3, col="red")
abline(h=-4, col="yellow")
abline(h=4, col="yellow")

Box.test(x = TSmodel.ar2$residuals, type = "Box-Pierce",lag=10) 

#the model is normal but didnt pass JB b/c of the existence of significant outliers or structure breaks
#Note: if an ARMA modle passes all disagnostics except for JB due to outliers, it is still a good mode

#overfit
TSsigma2.hat.overfit <- sum(TSmodel.ar6$residuals^2)/N
TSDev <- 179*log(TSsigma2.hat/TSsigma2.hat.overfit) 
TSDev
pval <- 1-pchisq(TSDev, 4)
pval
#do not reject

#Jarque Bera Test
TSskew<-sum(TSstdres^3)/N
TSkur<-sum(TSstdres^4)/N
TSJBstat<-N*(TSskew^2/6+(TSkur-3)^2/24)
TSt3 <- sqrt(N/6) * TSskew 
TSt4<- sqrt(N/24)* (TSkur-3)
TSt3
TSt4
TSt3^2 + TSt4^2
TSJBstat
#jarque.bera.test(Yt)
#JBstat > 6 so reject normality hypothesis 

#ARCH(6)
TSres <- TSmodel.ar2$residuals
TSres2 <- TSres^2
N4 <- length(TSres2)
ARCH6<-lm(TSres2[-(1:6)]^2~TSres2[-c(1:5,N4)]^2+TSres2[-c(1:4,(N4-1):N4)]^2
          +TSres2[-c(1,2,3,(N4-2):N4)]^2+TSres2[-c(1,2,(N4-3):N4)]^2
          +TSres2[-c(1,(N4-4):N4)]^2+TSres2[-((N4-5):N4)]^2)

R<-summary(ARCH6)$r.squared
LMstat<-N4*R
LMstat
1-pchisq(LMstat,6)
#LMstat is less than 0.872, 99% quantile of chi square 6 distribution, therefore p<0.01. So we reject the null
#hypothesis that there is no non-linear


#########DS##########
acf(Yt2)$acf
2/sqrt(length(Yt2))


DSmodel.ar1 <- lm(Yt2[-1] ~ Yt2[-length(Yt2)] - 1)
DSphi_11 <- as.numeric(DSmodel.ar1$coefficients)[1]
DSphi_11

DSphi_22 <- as.numeric(DSmodel.ar2$coefficients)[2]
DSphi_22

DSmodel.ar3 <- lm(Yt2[-(1:3)] ~ Yt2[-c(1:2, N2)] + Yt2[-c(1, (N2-1):N2)] + Yt2[-c((N2-2):N2)] - 1)
DSphi_33 <- as.numeric(DSmodel.ar3$coefficients)[3]
DSphi_33

DSmodel.ar4 <- lm(Yt2[-(1:4)] ~ Yt2[-c(1:3, N2)] + Yt2[-c(1:2, N2-1,N2)]
                  + Yt2[-c(1, (N2-2):N2)] + Yt2[-c((N2-3):N2)] - 1)
DSphi_44 <- as.numeric(DSmodel.ar4$coefficients)[4]
DSphi_44

DSmodel.ar5 <- lm(Yt2[-(1:5)] ~ Yt2[-c(1:4, N2)] + Yt2[-c(1:3, (N2-1): N2)]
                  + Yt2[-c(1:2, (N2-2):N2)] + + Yt2[-c(1, (N2-3):N2)] + Yt2[-c((N2-4):N2)] - 1)
DSphi_55 <- as.numeric(DSmodel.ar5$coefficients)[5]
DSphi_55

DSmodel.ar6 <- lm(Yt2[-(1:6)] ~ Yt2[-c(1:5, N2)] + Yt2[-c(1:4, N2-1,N2)]
                  + Yt2[-c(1:3, (N2-2):N2)] + Yt2[-c(1:2, (N2-3):N2)]
                  + Yt2[-c(1, (N2-4):N2)] + Yt2[-c((N2-5):N2)] - 1)
DSphi_66 <- as.numeric(DSmodel.ar6$coefficients)[6]
DSphi_66

DSmodel.ar7 <- lm(Yt2[-(1:7)] ~ Yt2[-c(1:6, N2)] + Yt2[-c(1:5, N2-1,N2)]
                  + Yt2[-c(1:4, (N2-2):N2)] + Yt2[-c(1:3, (N2-3):N2)]
                  + Yt2[-c(1:2, (N2-4):N2)] + Yt2[-c(1, (N2-5):N2)] + Yt2[-c((N2-6):N2)]- 1)
DSphi_77 <- as.numeric(DSmodel.ar7$coefficients)[7]
DSphi_77


DSmodel.ar8 <- lm(Yt2[-(1:8)] ~ Yt2[-c(1:7, N2)] + Yt2[-c(1:6, N2-1,N2)]
                  + Yt2[-c(1:5, (N2-2):N2)] + Yt2[-c(1:4, (N2-3):N2)]
                  + Yt2[-c(1:3, (N2-4):N2)] + Yt2[-c(1:2, (N2-5):N2)] +  Yt2[-c(1, (N2-6):N2)] +  Yt2[-c((N2-7):N2)]- 1)
DSphi_88 <- as.numeric(DSmodel.ar8$coefficients)[8]
DSphi_88


DSmodel.ar9 <- lm(Yt2[-(1:9)] ~ Yt2[-c(1:8, N2)] + Yt2[-c(1:7, N2-1,N2)]
                  + Yt2[-c(1:6, (N2-2):N2)] + Yt2[-c(1:5, (N2-3):N2)]
                  + Yt2[-c(1:4, (N2-4):N2)] + Yt2[-c(1:3, (N2-5):N2)] +  Yt2[-c(1:2, (N2-6):N2)] +  Yt2[-c(1, (N2-7):N2)] + Yt2[-c((N2-8):N2)]- 1)
DSphi_99 <- as.numeric(DSmodel.ar9$coefficients)[8]
DSphi_99


acf(Yt2)$acf[1:10]
2/sqrt(length(Yt2))
c(DSphi_11,DSphi_22,DSphi_33,DSphi_44,DSphi_55,DSphi_66,DSphi_77,DSphi_88,DSphi_99)



#####AR(1)#######
DSAR1sigma2.hat <- sum(DSmodel.ar1$residuals^2)/N2
DSAR1stdres <- DSmodel.ar1$residuals/(sqrt(DSAR1sigma2.hat))
plot(DSAR1stdres, ylab="Standardized Residuals", 
     xlab="Time", 
     main="DS AR1", ylim = c(-4.2,4.2))
abline(h=-2, col="blue")
abline(h=2, col="blue")
abline(h=-3, col="red")
abline(h=3, col="red")
abline(h=-4, col="yellow")
abline(h=4, col="yellow")
Box.test(x = DSmodel.ar1$residuals, type = "Box-Pierce",lag=10) 
#the model is normal but didnt pass JB b/c of the existence of significant outliers or structure breaks

#overfit AR(5)
DSAR1sigma2.hat.overfit <- sum(DSmodel.ar5$residuals^2)/N2
DSDev1 <- 180*log(DSAR1sigma2.hat/DSAR1sigma2.hat.overfit) 
DSDev1
pval <- 1-pchisq(DSDev1, 4)
pval
#do not reject

#Jarque Bera Test
DSAR1skew<-sum(DSAR1stdres^3)/N2
DSAR1kur<-sum(DSAR1stdres^4)/N2
DSAR1JBstat<-N2*(DSAR1skew^2/6+(DSAR1kur-3)^2/24)
DSt3 <- sqrt(N2/6) * DSAR1skew 
DSt4<- sqrt(N2/24)* (DSAR1kur-3)
DSt3
DSt4
DSt3^2 + DSt4^2
DSAR1JBstat
#jarque.bera.test(Yt)
#JBstat > 6 so reject normality hypothesis 

#ARCH(6)
# ARCH6<-lm(Yt[-(1:6)]^2~Yt[-c(1:5,N)]^2+Yt[-c(1:4,(N-1):N)]^2+Yt[-c(1,2,3,(N-2):N)]^2+Yt[-c(1,2,(N-3):N)]^2
#           +Yt[-c(1,(N-4):N)]^2+Yt[-((N-5):N)]^2)

DSAR1res <- DSmodel.ar1$residuals
DSAR1res2<-DSAR1res^2
N5 <- length(DSAR1res2)
ARCH6<-lm(DSAR1res2[-(1:6)]~DSAR1res2[-c(1:5,N5)]+DSAR1res2[-c(1:4,(N5-1):N5)]
          +DSAR1res2[-c(1,2,3,(N5-2):N5)]+DSAR1res2[-c(1,2,(N5-3):N5)]
          +DSAR1res2[-c(1,(N5-4):N5)]+DSAR1res2[-((N5-5):N5)])

R<-summary(ARCH6)$r.squared
R
LMstat<-N5*R
LMstat
1-pchisq(LMstat,6)


#####MA(3)######
DSmodel.ma3 <- arima(Yt2, order = c(0,0,3), include.mean = F)
DSMA3stdres <- DSmodel.ma3$residuals/(sqrt(DSmodel.ma3$sigma2))
plot(DSMA3stdres, ylab="Standardized Residuals", 
     xlab="Time", 
     main="DS MA3", ylim = c(-4.2,4.2), type ="p")
abline(h=-2, col="blue")
abline(h=2, col="blue")
abline(h=-3, col="red")
abline(h=3, col="red")
abline(h=-4, col="yellow")
abline(h=4, col="yellow")
Box.test(x = DSmodel.ma3$residuals, type = "Box-Pierce",lag=10) 
#the model is normal but didnt pass JB b/c of the existence of significant outliers or structure breaks

#overfit MA(7)
DSmodel.ma7 <- arima(Yt2, order = c(0,0,7), include.mean = F)
DSDev2 <- 180*log(DSmodel.ma3$sigma2/DSmodel.ma7$sigma2) 
DSDev2
pval <- 1-pchisq(DSDev2, 4)
pval
#do not reject

#Jarque Bera Test
DSMA3skew<-sum(DSMA3stdres^3)/N2
DSMA3kur<-sum(DSMA3stdres^4)/N2
DSMA3JBstat<-N2*(DSMA3skew^2/6+(DSMA3kur-3)^2/24)
DSt3 <- sqrt(N2/6) * DSMA3skew 
DSt4<- sqrt(N2/24)* (DSMA3kur-3)
DSt3
DSt4
DSt3^2 + DSt4^2
DSMA3JBstat
#jarque.bera.test(Yt)
#JBstat > 6 so reject normality hypothesis 

#ARCH(6)
# ARCH6<-lm(Yt[-(1:6)]^2~Yt[-c(1:5,N)]^2+Yt[-c(1:4,(N-1):N)]^2+Yt[-c(1,2,3,(N-2):N)]^2+Yt[-c(1,2,(N-3):N)]^2
#           +Yt[-c(1,(N-4):N)]^2+Yt[-((N-5):N)]^2)

DSMA3res <- DSmodel.ma3$residuals
DSMA3res2 <- DSMA3res^2
N6 <- length(DSMA3res2)
ARCH6<-lm(DSMA3res2[-(1:6)]~DSMA3res2[-c(1:5,N6)]+DSMA3res2[-c(1:4,(N6-1):N6)]+DSMA3res2[-c(1,2,3,(N6-2):N6)]+DSMA3res2[-c(1,2,(N6-3):N6)]
          +DSMA3res2[-c(1,(N6-4):N6)]+DSMA3res2[-((N6-5):N6)])

R<-summary(ARCH6)$r.squared
R
LMstat<-N6*R
LMstat
1-pchisq(LMstat,6)






###########8

SPdata<- read_excel("C:/alice/Waterloo Stuff/STUDYMATERIALS/05. STAT443/Project/S&P_data_for_Q8.xls")
Pt<-as.numeric(as.character(SPdata$P))
N3<-length(Pt)
SPlm<-lm(log(Pt[-1])~log(Pt[-N3]))
summary(SPlm)
2 * (1-pnorm((1.000014-1)/  0.001638))
N3
SPYt<-SPlm$residuals

acf(SPYt,type="correlation")$acf
2/sqrt(N3)
#no autocorrelation so independent

Box.test(x = SPlm$residuals, type = "Box-Pierce",lag=25) 

#std residual
SPstdres<-SPYt/sqrt(sum(resid(SPlm)^2)/N3)
plot(SPstdres, 
     ylab="Standardized Residuals", 
     xlab="Time", 
     main="SP", ylim = c(-4.2,4.2), type ="p")
abline(h=-2, col="blue")
abline(h=2, col="blue")
abline(h=-3, col="red")
abline(h=3, col="red")
abline(h=-4, col="yellow")
abline(h=4, col="yellow")

SPskew<-sum(SPstdres^3)/N3
SPkur<-sum(SPstdres^4)/N3
SPt3 <- sqrt(N2/6) * SPskew 
SPt4<- sqrt(N2/24)* (SPkur-3)
SPt3
SPt4
SPJBstat<-N3*(SPskew^2/6+(SPkur-3)^2/24)
SPJBstat

#autocorrelation for at^2
SPYtsquared<-SPYt^2
acf(SPYtsquared,type="correlation")$acf

garchFit(formula = ~garch(1, 1), data = SPYt)
