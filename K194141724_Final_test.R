# Import libraries
library(ggplot2) 
library(tidyverse)  
library(forcats) 
library(scales) 
library(pastecs)
library(summarytools)
require(tidyverse)
library("readxl")

# Load data set
data = read_excel('K194141724.xlsx')
quarter = data$quarter
data = subset(data, select = -c(quarter))
rownames(data) = quarter
View(data)
# Create leverage column
data$leverage = data$total_lia / data$total_equity
# Create cash ratio
data$cash_ratio = data$cash_and_equivalents / data$current_lia
# Create revenue growth rate
data$growth_rate=data$revenue/lag(data$revenue)-1
# Create ROA
data$roa = data$net_income / data$total_assets
# Create r_leverage
data$r_leverage=data$leverage/lag(data$leverage)-1
# Drop the first row which contains a missing value
data=na.omit(data)
# Split data set
data_before = data[1:39,]
data_after = data[40:47,]
View(data_before)
View(data_after)
# Descriptive statistics
# All period
descr(data[c('cash_ratio', 'future_fcf', 'growth_rate', 'roa', 'tangible_assets', 'leverage')], transpose = TRUE, stats=c('mean', 'min', 'med', 'max', 'sd'))
# Before period
descr(data_before[c('cash_ratio', 'future_fcf', 'growth_rate', 'roa', 'tangible_assets', 'leverage')], transpose = TRUE, stats=c('mean', 'min', 'med', 'max', 'sd'))
# After period
descr(data_after[c('cash_ratio', 'future_fcf', 'growth_rate', 'roa', 'tangible_assets', 'leverage')], transpose = TRUE, stats=c('mean', 'min', 'med', 'max', 'sd'))
# Create Covid-19 dummy variable
covid_unexist <- list(rep(0,39))
covid_exist <- list(rep(1,8))
data$covid = c(unlist(covid_unexist),unlist(covid_exist))
data$covid
# Box and whisker plot of leverage 
boxplot(data$leverage)
# Scatter plot for leverage with cash ratio, covid_19
ggplot(data, aes(x= cash_ratio, y= leverage, color = covid)) +geom_point()
# Scatter plot for leverage with growth rate, covid_19
ggplot(data, aes(x= growth_rate, y= leverage, color = covid)) +geom_point()
# Scatter plot for leverage with tangible assets, covid_19
ggplot(data, aes(x= tangible_assets, y= leverage, color = covid)) +geom_point()
# Scatter plot for leverage with ROA, covid_19
ggplot(data, aes(x= roa, y= leverage, color = covid)) +geom_point()
# Scatter plot for leverage with future FCF, covid_19
ggplot(data, aes(x= future_fcf, y= leverage, color = covid)) +geom_point()
# Histogram plot of leverage
ggplot(data, aes(x=leverage)) +
  geom_histogram(binwidth=.2, fill='cadetblue', color='steelblue4')
# Multiple regression with the usual individual variables 
leverage.model1<-lm(leverage~tangible_assets+cash_ratio+growth_rate+future_fcf+roa, data=data)
summary(leverage.model1)
# Interpretation
summary(leverage.model1)$coefficient
# Test linear relationship in data
par(mfrow=c(2,2))
plot(leverage.model1)
# Multiple regression with the usual individual variables and the interaction between Covid-19 dummy variable and the independent variables 
leverage.model2 <- lm(leverage~covid*tangible_assets+covid*cash_ratio+covid*growth_rate+covid*future_fcf+covid*roa, data=data)
summary(leverage.model2)
# Interpretation
summary(leverage.model2)$coefficient
# Test linear relationship in data
par(mfrow=c(2,2))
plot(leverage.model2)
# Predict the value of the variable of assigned topic for all the quarters of the sample using Model 1 
predict(leverage.model1, newdata = data)
# ARIMA model
library(forecast) #forecast, accuracy
library(quantmod) #getSymbols
library(tseries) #adf.test
library(lmtest) #coeftest
library(stats) #Box.test
# Plot variables
par(mfrow=c(2,1))
plot(data$leverage)
plot(data$r_leverage)
# Check stationary of leverage
par(mfrow=c(1,2))
acf(data$leverage,main='ACF for leverage')
pacf(data$leverage,main='PACF for leverage')
adf.test(data$leverage)       # p-value = 0.6269 --> not stationary
# Now check whether return of leverage is stationary
par(mfrow=c(1,2))
acf(data$r_leverage,main='ACF for return of leverage') 
pacf(data$r_leverage,main='PACF for return of leverage') 
adf.test(data$r_leverage)     # p-value = 0.01705  --> stationary
# Use auto.arima function to determine best P, D, Q
auto=auto.arima(data$r_leverage,seasonal=F,trace = T,max.order=4,ic='aic')
coeftest(auto.arima(data$r_leverage,seasonal=F))
acf(auto$residuals)
pacf(auto$residuals)
Box.test(auto$residuals,lag=20,type='Ljung-Box')       # X-squared = 14.447, df = 20, p-value = 0.8071
# Prediction
term=4
fcastauto=forecast(auto,h=term)
fcastauto          # fcastauto is the predicted values for 4 terms
par(mfrow=c(1,1))
plot(fcastauto)
# Test accuracy
accuracy(fcastauto) #train set   

