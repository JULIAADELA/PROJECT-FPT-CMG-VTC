
library(readxl)
CMG_GSR <- read_excel("C:/Users/ADMIN/Music/Năm3 - kì2/Time Series/CMG_GSR.xlsx",
                      sheet = "GROSS SALE REVENUE")
View(CMG_GSR)
save(CMG_GSR, file = "CMG_GSR.rda")
attach(CMG_GSR)
View(CMG_GSR)


#### 4.1: TIME TREND.


ts(CMG_GSR, start = c(2009,1),frequency = 4)
gsr <- ts(GSR, start = c(2009,1),frequency = 4)
plot.ts(gsr)

#### 4.1.1. Create Time Trend
time <- seq_along(gsr)
summary(time)

### 4.1.2. Linear-Linear
plot(time,gsr)
abline(lm(gsr~time))
reg412 <- lm(gsr~time)
summary(reg412)

# gsr_t= 727657 + 14278*t+u_t
# 2023Q1, t= 56+1:
727657 + 14278*57 # 1541503
# 2023Q2, t= 56+2:
727657 + 14278*58 # 1555781
# 2023Q3, t= 56+3:
727657 + 14278*59  #  1570059
# 2023Q4, t= 56+4:
727657 + 14278*60  # 1584337

library(Metrics)
rmse(gsr,fitted(reg412))  #468381.9
mape(gsr,fitted(reg412))  #0.2181371

#2022
rmse(gsr[53:56],fitted(reg412))  #836135.4
mape(gsr[53:56],fitted(reg412))  #  0.3981224
gsrf1 <- ts(fitted(reg412),start = c(2009,1), frequency = 4)
plot.ts(gsr)
lines(gsrf1, col = "red")

### 4.1.3. Linear-Log
reg413 <- lm(gsr~log(time))
summary(reg413)
# gsr_t = 545740 + 191319 *ln(t) + u_t
# 2023Q1, t= 56+1:
545740 + 191319*log(57)  # 1319253
# 2023Q2, t= 56+2:
545740 + 191319*log(58)  # 1322580
# 2023Q3, t= 56+3:
545740 + 191319*log(59)  # 1325850
# 2023Q4, t= 56+4:
545740 + 191319*log(60)  # 1329066

rmse(gsr,fitted(reg413))  # 493703
mape(gsr,fitted(reg413))  # 0.265589
#2022
rmse(gsr[53:56],fitted(reg413))  # 821571
mape(gsr[53:56],fitted(reg413))  # 0.3980945

gsrf2 <- ts(fitted(reg413),start = c(2009,1), frequency = 4)
plot.ts(gsr)
lines(gsrf2, col = "blue")


### 4.1.4. Log-linear
reg414 <- lm(log(gsr)~time)
summary(reg414)
# ln(gsr_t) = 13.445942 + 0.014653*t +u_t
# # 2023Q1, t= 56+1: 1593053
exp(13.445942 + 0.014653*57)
# 2023Q2, t= 56+2:1616568
exp(13.445942 + 0.014653*58)
# 2023Q3, t= 56+3:1640430
exp(13.445942 + 0.014653*59)
# 2023Q4, t= 56+4: 1664644
exp(13.445942 + 0.014653*60)

library(Metrics)
rmse(gsr,exp(fitted(reg414)))  # 463439.6
mape(gsr,exp(fitted(lm(reg414))))  #  0.1797915
#2022
rmse(gsr[53:56],exp(fitted(reg414)))  # 893856.7
mape(gsr[53:56],exp(fitted(lm(reg414))))  # 0.4277838

gsrf3 <- ts(exp(fitted(reg414)),start = c(2009,1), frequency = 4)
plot.ts(gsr)
lines(gsrf3, col = "green")

### 4.1.5. Log-log
reg415 <- lm(log(gsr)~log(time))
summary(reg415)
# ln(gsr_t) = 13.2114  + 0.2119 ln(t) + u_t
exp(13.2114)*57^(0.2119)  #  1287384
exp(13.2114)*58^(0.2119)  #  1292137
exp(13.2114)*59^(0.2119)  # 1296826
exp(13.2114)*60^(0.2119)  # 1301453

rmse(gsr,exp(fitted(reg415)))   # 493290.1
mape(gsr,exp(fitted(reg415))) # 0.2283289
# 2022
rmse(gsr[53:56],exp(fitted(reg415)))   #  887633.9
mape(gsr[53:56],exp(fitted(reg415)))# 0.4342756

gsrf4 <- ts(exp(fitted(reg415)),start = c(2009,1), frequency = 4)
plot.ts(gsr)
lines(gsrf4, col = "purple")
#So sánh các mô hình





#################### SEASONAL-DUMMIES ################################
### create quarterly dummies
s1<-c(rep(c(1, 0, 0, 0), 14))
s2<-c(rep(c(0, 1, 0, 0), 14))
s3<-c(rep(c(0, 0, 1, 0), 14))
s4<-c(rep(c(0, 0, 0, 1), 14))
reg42b <- lm(gsr ~ s2+s3+s4)
summary(reg42b)

library(tsutils)
seas <- seasdummy(56,4)

###  Regression with seasonal dummies
summary(lm(gsr ~ time + seas))

########################################################################################
library(Metrics)
## I - Linear trend + seasonality

## 1. Additive form
summary(lm(gsr ~ time + s2+s3+s4))
# Y_t = 576035+13524*t+32127*s2+186775*s3+473507*s4
# 2023Q1, t= 56+1, s1=1, s2=s3=s4=0:
576035 + 13524*57            # 1346903
# 2023Q2, t= 56+2, s2=1:
576035 + 13524*58 + 32127    # 1392554
# 2023Q3, t= 56+3, s3=1:
576035 + 13524*59 + 186775   # 1560726
# 2023Q4, t= 56+4, s4=1:
576035 + 13524*60 + 473507   # 1860982

summary(lm(gsr ~ time + seas))


reg1 <- lm(gsr ~ time + s2+s3+s4)   #
mape(gsr, fitted(reg1))       # 0.1945024
rmse(gsr,fitted(reg1)) # 429491.4
#2022
mape(gsr[53:56], fitted(reg1))      #0.4085105
rmse(gsr[53:56],fitted(reg1)) # 806650.9
        
decom.gsr.a<-decompose(gsr,type='additive')
plot(decom.gsr.a)
gsr.trend.a <- decom.gsr.a$trend
gsr.seas.a <- decom.gsr.a$seasonal
gsr.rand.a <- decom.gsr.a$random
gsr.trendseas.a <- (gsr - gsr.rand.a)
## graph
plot(gsr,col='blue')
lines(gsr.trend.a)
lines(gsr.trendseas.a,col = "red")

## 2. Multiplicative form
summary(lm(gsr ~ time + time*s2 + time*s3+ time*s4))
# Y_t = 439779 + 18570*t + 116119*s2 + 208988*s3 + 935130*s4 - 3180*t*s2 - 1114*t*s3 -15892*t*s4
# 2023 Q1, t= 57, s2=s3=s4 = 0
439779 + 18570*57                           # 1498269
# 2023Q2, t=58, s2=1,   s1=s3=s4 = 0
439779 + 18570*58 + 116119*1 - 3180*58*1    #  1448518
# 2023Q2, t=59, s3=1,   s1=s2=s4 = 0
439779 + 18570*58 + 208988*1 - 1114*59*1    #  1660101
# 2023Q2, t=60, s4=1,   s1=s2=s3 = 0
439779 + 18570*58 + 935130*1 -15892*60*1    #  1498449
        

reg2 <- lm(gsr ~ time + time*s2 + time*s3+ time*s4)#
rmse(gsr,fitted(reg2)) # 417049.2
mape(gsr, fitted(reg2))    # 0.1977193

#2022

rmse(gsr[53:56],fitted(reg2))# 813153.4
mape(gsr[53:56], fitted(reg2))    # 0.4085105

decom.gsr.a<-decompose(gsr,type='additive')
plot(decom.gsr.a)
gsr.trend.a <- decom.gsr.a$trend
gsr.seas.a <- decom.gsr.a$seasonal
gsr.rand.a <- decom.gsr.a$random
gsr.trendseas.a <- (gsr - gsr.rand.a)
## graph
plot(gsr,col='blue')
lines(gsr.trend.a)
lines(gsr.trendseas.a,col = "red")

## II - Non-linear trend + seasonality
## 1.Additive form
summary(lm(log(gsr)~time + s2 + s3 + s4))
# ***** mape *****
reg3 <- lm(log(gsr)~time + s2 + s3 + s4)
mape(gsr[1:55], exp(fitted(reg3)))

## 2. Multiplicative form
summary(lm(log(gsr)~time + time*s2 + time*s3 + time*s4))
reg4 <- lm(log(gsr)~time + time*s2 + time*s3 + time*s4)
mape(gsr[1:55], exp(fitted(reg4)))   # 0.1643412

##########################################################################################

## I - Holt-Winters analysis
## 1. Holt-Winter with seasonality (Additive form) 
gsr<-ts(gsr,start=c(2009,1),frequency = 4)
hw.gdp.a <- HoltWinters(gsr, seasonal = "a") # a = additive
hw.gdp.a
# Y_t = 1984722.7 + 43389.78*k +s(1/2/3/4)
# 2023Q1, t= 56+1
1984722.7 + 43389.78*1 -196751.761   # 1831361
# 2023Q2 , t= 56+2
1984722.7 + 43389.78*2 -231029.35  # 1840473
# 2023Q3 , t= 56+3
1984722.7 + 43389.78*3 -38621.53  # 2076271
# 2023Q4 , t= 56+4
1984722.7 + 43389.78*4 + 227851.69   # 2386134



plot.ts(gsr)
lines(fitted(hw.gdp.a)[,1], col = "red")
rmse(gsr,fitted(hw.gdp.a))   #966781.2
mape(gsr,fitted(hw.gdp.a))#  0.6441375
#2022
rmse(gsr[53:56],fitted(hw.gdp.a)) #1467855
mape(gsr[53:56], fitted(hw.gdp.a))     #  0.6901427

## 2. Holt-Winter with seasonality (Multiplicative form)
hw.gdp.m <- HoltWinters(gsr, seasonal = "m") 
hw.gdp.m
# Y_t = (2176651 + 1.001781e+05*k)*S
# 2023Q1, t= 56+1
(2176651 + 100178.1*1)*0.8742666 #  1990556
(2176651 + 1.001781e+05*2)* 8.431510e-01 #2004176
(2176651 + 1.001781e+05*3)*9.386350e-01# 2325173
(2176651 + 1.001781e+05*4)* 1.041021e+00 # 2683089

plot.ts(gsr)
lines(fitted(hw.gdp.m)[,1], col = "red")
rmse(gsr,fitted(hw.gdp.m)) #  939495.5
mape(gsr,fitted(hw.gdp.m)) #  0.604778

#2022
rmse(gsr[53:56],fitted(hw.gdp.m)) # 1470037
mape(gsr[53:56], fitted(hw.gdp.m))     # 0.6889684
