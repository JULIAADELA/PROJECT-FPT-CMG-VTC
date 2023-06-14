library(readxl)
CMG_GSR <- read_excel("C:/Users/ADMIN/Music/Năm3 - kì2/Time Series/CMG_GSR.xlsx", 
                      sheet = "DAILYDATA")
View(CMG_GSR)

cmg <- CMG_GSR$CMG

plot.ts(cmg)
plot.ts(diff(cmg))
time <-seq_along(cmg)
cmg.detrend <- resid(lm(cmg ~ time))
plot.ts(cmg.detrend)
library(urca)
#unit root test
summary(ur.df(cmg,type = "trend"))


#ADF test for ∆cmg_t:
# + With trend
summary(ur.df(diff(cmg),type = "trend"))

# Unit Root test for Difference series
summary(ur.df(diff(cmg),type = "drift"))


acf(diff(cmg))
pacf(diff(cmg))
library(forecast)
#### Estimate ARIMA(1,1,3) model
reg.cmg.arima113 <- Arima(cmg, order = c(1,1,3), include.constant =  TRUE)
summary(reg.cmg.arima113)


#### Estimate ARIMA(1,1,2) model

reg.cmg.arima112 <- Arima(cmg, order = c(1,1,2), include.constant =  TRUE)
summary(reg.cmg.arima112)
# MAPE : 1.965707
# RMSE : 1.208153

autoplot(reg.cmg.arima112)

#####
checkresiduals(reg.cmg.arima112)

# forecast 10 obs in 2023  
library(Metrics)
cmgf.arima112 <- forecast(reg.cmg.arima112,h=10)
summary(cmgf.arima112)

mape(cmg, fitted(cmgf.arima112))       #  0.01965707
rmse(cmg, fitted(cmgf.arima112))       # 1.208153
 



# ∆cmg_t -(- 0.0159) = -0.1874(∆cmg_t-1 -0.0159) + u_t + 0.1655u_t-1 -0.0827u_t-2
#  (μ^* ) = -0.0159    => μ^ = -0.0159(1--0.1874) = -0.01887966
-0.0159*(1--0.1874)
# ∆cmg_t =  -0.01887 + (-0.1874)∆cmg_t-1 + u_t + 0.1655u_t-1 -0.0827u_t-2
