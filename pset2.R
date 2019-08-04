#Giancarlo Carino
#EC 410
#Problem Set 2

#Install packages
install.packages(pkgs=c("psych", "stargazer", "lmtest", "car", "dplyr","readxl"))
library(psych)
library(stargazer)
library(lmtest)
library(car)
library(dplyr)
library(readxl)

#Import
setwd("/Users/gc/Desktop/GitHub/EC-410-Food-and-Agricultural-Economics/Problem Set 2/data")
pset2data <- read_excel("HW2_data.xlsx")
attach(pset2data)

#Convert nominal to real (Base period: December 2011)
pset2data$real_P_Icecream <- (pset2data$P_Icecream/pset2data$Food_CPI)*229.982
pset2data$real_P_Milk <- (pset2data$P_Milk/pset2data$Food_CPI)*229.982
pset2data$real_P_Butter <- (pset2data$P_Butter/pset2data$Food_CPI)*229.982

#Create per capita supply of icecream
pset2data$percap_Q_Icecream <- (pset2data$Q_Icecream / pset2data$POP)

#Order/Sort by year and month
pset2data <- pset2data[with(pset2data, order(Year, Month)),]

#Create time series variable t
pset2data$t <- time(pset2data$Q_Icecream)

#Convert vector Q_Icecream into time series object
pset2data$Q_Icecream <- ts(pset2data$Q_Icecream, start = c(1990, 1), end = c(2011,12), frequency = 12)
plot.ts(pset2data$Q_Icecream, ylab = "Demand for Ice Cream")

#Linear regression model (DV: percap_Q_Icecream, INDV: real_P_Icecream, real_P_Milk, real_P_Butter, t)
reg <- lm(percap_Q_Icecream ~ real_P_Icecream + real_P_Butter + real_P_Milk + t, data = pset2data, na.action = na.exclude)

summary(reg)

#Summary stats
stargazer(
  as.data.frame(pset2data[c("percap_Q_Icecream", "real_P_Icecream", "real_P_Butter", "real_P_Milk")]), type="text", title="Descriptive Statistics", digits=2, out="table.htm", covariate.labels=c("Per Capita Supply of Ice Cream", "Real Price of Ice Cream", "Real Price of Butter", "Real Price of Milk"), summary.stat=c("n", "mean", "sd", "min", "max")
)

#Create Dummy Variable 2007
pset2data$y2007 <- 0
pset2data$y2007 <- ifelse(Year >= 2007, 1, 0)

#Set up lag variable prices
pset2data$lag_real_P_Icecream <- lag(pset2data$real_P_Icecream, n = 1L)
pset2data$lag_real_P_Butter <- lag(pset2data$real_P_Butter, n = 1L)
pset2data$lag_real_P_Milk <- lag(pset2data$real_P_Milk, n = 1L)

#New Regression model(DV: percap_Q_Icecream, INDV: real_P_Icecream, real_P_Milk, real_P_Butter, t, 2007, lag_real_P_Icecream, lag_real_P_Milk, lag_real_PButter)

reg2 <- lm(percap_Q_Icecream ~ real_P_Icecream + real_P_Butter + real_P_Milk + t + y2007 + lag_real_P_Icecream + lag_real_P_Butter + lag_real_P_Milk , data = pset2data, na.action = na.exclude)
summary(reg2)

#Test for Heteroscedasticity
#Create residuals
pset2data$residuals <- resid(reg2)

#Plot Residuals vs INDV: real price of Ice cream, real price of Butter, real price of Milk
plot(pset2data$real_P_Icecream, pset2data$residuals, ylab = "Residuals", xlab = "Real Price of Ice Cream", main = "Residuals vs the real price of Ice Cream")
plot(pset2data$real_P_Butter, pset2data$residuals, ylab = "Residuals", xlab = "Real Price of Butter", main = "Residuals vs the real price of Butter")
plot(pset2data$real_P_Milk, pset2data$residuals, ylab = "Residuals", xlab = "Real Price of Milk", main = "Residuals vs the real price of Milk")
plot(pset2data$t, pset2data$residuals, ylab = "Residuals", xlab = "Linear Time Trend", main = "Residuals vs Linear Time Trend")

bptest(reg2)

#Test for multicollinearity
#Create lag of residuals
pset2data$lag_resid <- lag(pset2data$residuals, n=1L)
#Plot a scatterplot of residuals vs lags
plot(pset2data$lag_resid, pset2data$residuals, ylab="Residuals", xlab="Lag", main="Residuals vs their lag")
#Plot a scatterplot of residuals vs time
plot(pset2data$t, pset2data$residuals, ylab="Residuals", xlab="Time", main="Residuals vs Time")

vif(reg2)

#Test for autocorrelation
dwtest(reg2)

