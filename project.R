library(astsa)
library(TSA)
library(forecast)
library(dplyr)
library(vars)
library(corrplot)
library(tidyr)
library(ggplot2)


df = read.csv('https://dxl-datasets.s3.amazonaws.com/data/industrial_data.csv')
openings = ts(df$openings, start=c(2002, 1), frequency=12)
layoffs = ts(df$layoffs, start=c(2002, 1), frequency=12)
spend = ts(df$spend, start=c(2002, 1), frequency=12)
employees = ts(df$employees, start=c(2002, 1), frequency=12)


X = cbind(openings, layoffs, spend, employees)
autoplot(X, facets=T)

# 1) Linear Regression 
m_lin = tslm(openings ~ poly(trend, 3)+season)

summary(m_lin)

f_lin = forecast(m_lin, h=12)$mean

autoplot(openings) + 
  autolayer(f_lin)

# 2) Seasonal Trend Decomposition using Loess (STL)
m_stl = stl(openings, s.window = 'periodic')

f_stl = forecast(m_stl, h=12)$mean

autoplot(openings) + 
  autolayer(f_stl)

# 3) ETS Smoothing method similar to STL but using exponential smoothing instead of LOESS / local regressions
m_ets = ets(openings, model='ZZZ')

f_ets = forecast::forecast(m_ets, h=12)$mean

autoplot(openings) + 
  autolayer(f_ets)

# 4) Neural Network Autoregression 
m_nnet = nnetar(openings, size=10)

f_nnet = forecast::forecast(m_nnet, h=12)$mean

autoplot(openings) + 
  autolayer(f_nnet)

# 5) Vector Autoregression 
m_var = VAR(cbind(openings, layoffs, spend, employees),
            p=4,
            season = 12)

f_all_var = predict(m_var, n.ahead=12)
fanchart(f_all_var)
f_var = ts(f_all_var$fcst$openings[, 1], start=c(2022, 1), frequency=12)

autoplot(openings) + 
  autolayer(f_var)

# 6) ARIMA
plot(openings)

BoxCox.ar(openings, method = 'burg')

plot(diff(sqrt(openings)))

plot(diff(diff(sqrt(openings), lag = 12)))

x = diff(diff(sqrt(openings), lag = 12))

acf2(x)
eacf(x)

sarima(sqrt(openings), 0,1,1,0,1,1,12)

m_arima = Arima(openings, order = c(0,1,1), seasonal = c(0,1,1), include.constant = T)

f_arima = forecast::forecast(m_arima, h = 12)$mean

autoplot(openings) + 
  autolayer(f_arima)


# Forecast Results
autoplot(openings) + 
  autolayer(f_lin) +
  autolayer(f_stl) +
  autolayer(f_ets) + 
  autolayer(f_arima) + 
  autolayer(f_var) + 
  autolayer(f_nnet) 


# Weighted Averaging

w_arima <- 0.2
w_ets <- 0.2
w_lin <- 0.15
w_nnet <- 0.1
w_stl <- 0.2
w_var <- 0.15

f_weighted_avg <- w_arima*f_arima + 
  w_ets*f_ets + 
  w_lin*f_lin + 
  w_nnet*f_nnet + 
  w_stl*f_stl + 
  w_var*f_var

autoplot(openings) +
  autolayer(f_weighted_avg) 


ts_data = data.frame(f_weighted_avg)
write.csv(ts_data,'Project.csv', row.names = T)