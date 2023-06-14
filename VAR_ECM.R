library(readxl)
ECM <- read_excel("C:/Users/ADMIN/Music/Năm3 - kì2/Time Series/CMG_GSR.xlsx",
                      sheet = "GROUP DAILY")
View(ECM)
save(ECM, file = "ECM.rda")
attach(ECM)
View(ECM)

library(urca)
plot.ts(data.frame(CMG,FPT,VTC))
library(forecast)
## Johansen test
# using trace criteria
summary(ca.jo(data.frame(CMG,FPT,VTC),type='trace'))

# with “eigen value” criteria
#summary(ca.jo(data.frame(CMG,FPT,VTC),type = "eigen"))

# unit root test
summary(ur.df(diff(CMG), type = "none"))
summary(ur.df(diff(FPT), type = "none"))
summary(ur.df(diff(VTC), type = "none"))


## test for cointegration
# esimate model
summary(lm(CMG ~ FPT + VTC))
# CMG_t = -15.59397 + 0.55168*FPT_t + 1.33372 *VTC_t


# Stationary Residual test
resid.CMG <- resid(lm(CMG ~ FPT + VTC))
summary(ur.df(resid.CMG, type = "none"))   
#  => NON UNIT ROOT, STATIONARY

### Error Correction Model
length(CMG)
summary(lm(diff(CMG) ~ diff(FPT) + diff(VTC) + resid.CMG[1:498]))

#### 8. VAR
cmg <- ECM$CMG
fpt <- ECM$FPT
vtc<- ECM$VTC
d.cmg <- diff(cmg)
d.fpt <- diff(fpt)
d.vtc <- diff(vtc)
data1 <- data.frame(d.cmg,d.fpt,d.vtc)
#### Granger causality test
library(lmtest)
grangertest(d.cmg, d.fpt, order = 1) #no
grangertest(d.cmg, d.fpt, order = 2)#no

grangertest(d.cmg, d.vtc, order = 1)#no
grangertest(d.cmg, d.vtc, order = 2)#granger

grangertest( d.fpt, d.vtc, order = 1)#no
grangertest(d.fpt,d.vtc, order = 2)#granger

grangertest( d.fpt,d.cmg, order = 1)# granger
grangertest( d.fpt, d.cmg,order = 2)# granger

grangertest( d.vtc, d.cmg,order = 1)  # granger
grangertest( d.vtc,d.cmg, order = 2)   # granger

grangertest(  d.vtc,d.fpt, order = 1)#no
grangertest(d.vtc, d.fpt,order = 2)#no

### VAR(2)
library(vars)
var2 <- VAR(data1, p =2, type = "const")
summary(var2)
# forecast
forecast1<-predict(var2)
plot(forecast1)
# IRF
irf(var2)
plot(irf(var2))
