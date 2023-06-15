library(astsa)
library(TSA)
library(forecast)
library(dplyr)
library(vars)
library(corrplot)
library(tidyr)
library(ggplot2)

#data cleaning
df = read.csv("interview_use_case.csv")
df2 = na.omit(df)

# Calculate monthly mean value
data_mean = rowMeans(df2[,4:72], na.rm = TRUE)

# Add the row means to the original data, order by mean
df2$mean = data_mean
df2 = df2 %>% arrange(desc(mean))

# There are some product have exactly same categories but different numerical values, I will merge them to one. 
df2 = df2 %>%
  group_by(Category1, Category2, Category3) %>%
  summarize(across(X10.Dec:mean, sum, na.rm = TRUE))%>%
  arrange(desc(mean))

# Get dataset group by Category1 and 2
df3 = df2 %>%
  group_by(Category1, Category2) %>%
  summarize(across(X10.Dec:mean, sum, na.rm = TRUE))%>%
  arrange(desc(mean))

# Get dataset group by Category1
df4 =df3%>%
  group_by(Category1) %>%
  summarize(across(X10.Dec:mean, sum, na.rm = TRUE)) %>%
  arrange(desc(mean))

# Get dataset ready for time serires
df21 <- df2 %>%
  unite(category, Category1:Category3, sep = "") %>%
  t() %>%
  `colnames<-`(.[1,]) %>%
  .[-1,]
df21 <- as.data.frame(apply(df21, 2, as.integer))

df31 <- df3 %>%
  unite(category, Category1:Category2, sep = "") %>%
  t() %>%
  `colnames<-`(.[1,]) %>%
  .[-1,]
df31 <- as.data.frame(apply(df31, 2, as.integer))

# Want to predict AAM, BCW, CCW, AA
#transform dataset to fit into time serires

aam = ts(df21$AAM[26:70], start = c(2013,1), frequency = 12)
bcw = ts(df21$BCW[26:70], start = c(2013,1), frequency = 12)
ccw = ts(df21$CCW[26:70], start = c(2013,1), frequency = 12)
aa = ts(df31$AA[26:70], start = c(2013,1), frequency = 12)

aae = ts(df21$AAE[26:70], start = c(2013,1), frequency = 12)
aab = ts(df21$AAB[26:70], start = c(2013,1), frequency = 12)
aaa = ts(df21$AAA[26:70], start = c(2013,1), frequency = 12)
aac = ts(df21$AAC[26:70], start = c(2013,1), frequency = 12)
aav = ts(df21$AAV[26:70], start = c(2013,1), frequency = 12)




# Check the plots
X1 = cbind(aam, bcw, ccw, aa)
autoplot(X1, facets=T)
X2 = cbind(aam, aae, aab, aaa, aac, aav)
autoplot(X2, facets=T)
X3 = cbind(aae, bcw)
autoplot(X3, facets=T)

# X2,X3 Correlation Matrix
cor1=cor(X2)
corrplot(cor1, method="color")
cor2=cor(X3)




# Building Model
# AAM
# Vector Autoregression for AAM
m_var1 = VAR(X2, p=1, season = 12)
f_all_var1 = predict(m_var1, n.ahead=12)

fanchart(f_all_var1, nc=2)

aam_var = ts(f_all_var1$fcst$aam[, 1], start=c(2016, 10), frequency=12)

autoplot(aam) + 
  autolayer(aam_var)


# ARIMA with differencing for AAM
plot(diff(aam))
plot(diff(diff(aam), lag = 12))

auto.arima(aam)

sarima(aam, 0, 0, 0, 0, 1, 0, 12)

acf2(aam)
eacf(aam)

aam_arima = Arima(aam, order = c(0,0,0), seasonal = c(0,1,0), include.constant = T)

aam_f = forecast(aam_arima, h = 12)$mean

autoplot(aam) + 
  autolayer(aam_f) 


# Seasonal Trend Decomposition using Loess (STL) for AAM
aam_stl = stl(aam, s.window = 'periodic')

aam_stl_f = forecast(aam_stl, h=12)$mean

autoplot(aam) + 
  autolayer(aam_stl_f) 



# BCW
# Vector Autoregression for BCW
m_var2 = VAR(X3, p=1, season = 12)
f_all_var2 = predict(m_var2, n.ahead=12)

fanchart(f_all_var2)

bcw_var = ts(f_all_var2$fcst$bcw[, 1], start=c(2016, 10), frequency=12)

autoplot(bcw) + 
  autolayer(bcw_var) 

# ARIMA with differencing for BCW
plot(diff(bcw))
plot(diff(diff(bcw), lag = 12))

auto.arima(bcw)

sarima(bcw, 0, 1, 1, 0, 1, 1, 12)

acf2(bcw)
eacf(bcw)

bcw_arima = Arima(bcw, order = c(0,1,1), seasonal = c(0,1,1), include.constant = T)

bcw_f = forecast(bcw_arima, h = 12)$mean

autoplot(bcw) + 
  autolayer(bcw_f) 

# Seasonal Trend Decomposition using Loess (STL) for BCW
bcw_stl = stl(bcw, s.window = 'periodic')

bcw_stl_f = forecast(bcw_stl, h=12)$mean

autoplot(bcw) + 
  autolayer(bcw_stl_f) 



# CCW
# ARIMA with differencing for CCW
plot(diff(ccw))
plot(ccw, lag = 12)

auto.arima(ccw)

sarima(ccw, 0, 1, 0, 0, 1, 0, 12)

acf2(ccw)
eacf(ccw)

ccw_arima = Arima(ccw, order = c(0,1,0), seasonal = c(0,1,0), include.constant = T)

ccw_f = forecast(ccw_arima, h = 12)$mean

autoplot(ccw) + 
  autolayer(ccw_f) 

# Seasonal Trend Decomposition using Loess (STL) for CCW
ccw_stl = stl(ccw, s.window = 'periodic')

ccw_stl_f = forecast(ccw_stl, h=12)$mean

autoplot(ccw) + 
  autolayer(ccw_stl_f) 


# AA
# ARIMA with differencing for AA
plot(diff(aa))
plot(diff(diff(aa), lag = 12))

auto.arima(aa)

sarima(aa, 0, 1, 0, 0, 1, 0, 12)

acf2(aa)
eacf(aa)

aa_arima = Arima(aa, order = c(0,1,0), seasonal = c(0,1,0), include.constant = T)

aa_f = forecast(aa_arima, h = 12)$mean

autoplot(aa) + 
  autolayer(aa_f) 

# Seasonal Trend Decomposition using Loess (STL) for AA
aa_stl = stl(aa, s.window = 'periodic')

aa_stl_f = forecast(aa_stl, h=12)$mean

autoplot(aa) + 
  autolayer(aa_stl_f) 




# Prediction Plot and Result
# AAM
autoplot(aam) + 
  autolayer(aam_f) + 
  autolayer(aam_stl_f) +
  autolayer(aam_var)
aam_pred = (aam_f + aam_stl_f + aam_var)/3
autoplot(aam) + 
  autolayer(aam_pred) 

# BCW
autoplot(bcw) + 
  autolayer(bcw_f) + 
  autolayer(bcw_stl_f) +
  autolayer(bcw_var)
bcw_pred = (bcw_f + bcw_stl_f + bcw_var)/3
autoplot(bcw) + 
  autolayer(bcw_pred) 

# CCW
autoplot(ccw) + 
  autolayer(ccw_f) + 
  autolayer(ccw_stl_f) 
ccw_pred = (ccw_f + ccw_stl_f)/2
autoplot(ccw) + 
  autolayer(ccw_pred) 

# AA
autoplot(aa) + 
  autolayer(aa_f) + 
  autolayer(aa_stl_f) 
aa_pred = (aa_f + aa_stl_f)/2
autoplot(aa) + 
  autolayer(aa_pred) 


ts_data = data.frame(aam_pred, bcw_pred, ccw_pred, aa_pred)
write.csv(ts_data,'Zepeng_Cui_Project.csv', row.names = T)
