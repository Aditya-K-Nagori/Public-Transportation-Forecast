library(readxl)
library(forecast)
library(ggplot2)
library(tidyverse)
library(lubridate)
setwd("C:/Users/91799/Downloads")
bicup.data<-read_excel("bicup2006.xls")


data = data.frame(bicup.data)
data$DATE=as.Date(data$DATE)
data$TIME=format(data$TIME, format = "%H:%M")
data$DATETIME <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y-%m-%d %H:%M")

summary(data)


# Visualize the demand over time
ggplot(data, aes(x = DATETIME, y = DEMAND)) +
  geom_line() +
  labs(title = "Demand over Time", x = "Date and Time", y = "Demand")

# plot the distribution of demand
ggplot(data, aes(x = DEMAND)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Demand", x = "Demand", y = "Frequency")

data$HOUR <-(format(data$TIME,format =  "%H"))
ggplot(data, aes(x = HOUR, y = DEMAND)) +
  geom_boxplot() +
  labs(title = "Boxplot of DEMAND by hour of day", x = "Hour of day", y = "DEMAND")

data0 <- data %>% 
  mutate(MOV_AVG = zoo::rollmeanr(DEMAND, k = 5, fill = NA))

ggplot(data0, aes(x = DATETIME)) +
  geom_line(aes(y = DEMAND), color = "blue", alpha = 0.7) +
  geom_line(aes(y = MOV_AVG), color = "red", size = 1) +
  labs(title = "Demand over Time with a Moving Average", x = "Time", y = "Demand")

# Plot demand by hour of the day
data1 <- data %>%
  mutate(HOUR = hour(DATETIME)) %>%
  group_by(HOUR) %>%
  summarise(AVG_DEMAND = mean(DEMAND), MED_DEMAND = median(DEMAND))

ggplot(data1, aes(x = HOUR, y = AVG_DEMAND)) +
  geom_col(fill = "lightblue") +
  geom_point(aes(y = MED_DEMAND), color = "red") +
  labs(title = "Demand by Hour of the Day", x = "Hour of the Day", y = "Average Demand")

# Plot demand by day of the week
data2 <- data %>%
  mutate(DAY = wday(DATETIME, label = TRUE)) %>%
  group_by(DAY) %>%
  summarise(AVG_DEMAND = mean(DEMAND), MED_DEMAND = median(DEMAND))

ggplot(data2, aes(x = DAY, y = AVG_DEMAND)) +
  geom_col(fill = "lightblue") +
  geom_point(aes(y = MED_DEMAND), color = "red") +
  labs(title = "Demand by Day of the Week", x = "Day of the Week", y = "Average Demand")

# Plot a boxplot of demand by hour of the day
ggplot(data1, aes(x = HOUR, y = DEMAND)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Demand by Hour of the Day", x = "Hour of the Day", y = "Demand")

# Plot a boxplot of demand by day of the week
ggplot(data2, aes(x = DAY, y = DEMAND)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Demand by Day of the Week", x = "Day of the Week", y = "Demand")


bicup.data<-read_excel("bicup2006.xls")
bicup.data.ts <- ts(bicup.data$DEMAND, start= c(1,1), frequency = 63)

acf(bicup.data.ts) #strong autocorrelation present

plot(bicup.data.ts, main = "Public Transportaion System",)

#trend, season and level present
plot(stl(bicup.data.ts, s.window = "periodic"), main = "Public Transportaion System")

#partition the data
nValid = 63*7                       
nTraining = length(bicup.data.ts)-nValid
bicup.train.ts = window(bicup.data.ts, end = c(1,nTraining))
bicup.valid.ts = window(bicup.data.ts, start = c(1,nTraining+1))

#Regression based model
train.lm.season = tslm(bicup.train.ts~season)
train.trend.season.pred = forecast(train.lm.season, h=63*7)

plot(train.lm.season$residuals)

model1=accuracy(train.lm.season,bicup.valid.ts)

train.lm.season.forecast=forecast(train.lm.season, h=63*10)

plot(train.lm.season.forecast$mean, main = "Regression based Forecast", xlim= c(2,25), xlab = "time", ylab = "Demand", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")
legend("topleft", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("blue", "green", "red"))

#holt winter's
bicup.data.hw = HoltWinters(bicup.train.ts, beta=FALSE)
autoplot(forecast(bicup.data.hw)) + xlab("Time") + ylab("Number of passengers") +
  ggtitle("Forecast of Demand using Holt-Winter's Exponential Smoothing")

bicup.data.hw.forecast = forecast(bicup.data.hw,h=63*7)

acf(bicup.data.hw.forecast$residuals, na.action = na.pass)

model2=accuracy(bicup.data.hw.forecast,bicup.valid.ts)

bicup.data.hw.forecast1 = forecast(bicup.data.hw,h=63*10)
plot(bicup.data.hw.forecast1$mean, main = "Holt Winter's Forecast", xlim= c(1,25), xlab = "Date", ylab = "Value", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")
legend("topleft", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("blue", "green", "red"))

#ARIMA model
bicup.data.diff=diff(bicup.train.ts,lag=63)

plot(bicup.data.diff)

acf(bicup.data.diff)
pacf(bicup.data.diff)

bicup.data.res.arima <- Arima(train.lm.season$residuals, order = c(1,1,2))
bicup.data.res.arima1 <- Arima(train.lm.season$residuals, order = c(0,2,0))

summary(bicup.data.res.arima)
tsdiag(bicup.data.res.arima)

summary(bicup.data.res.arima1)
tsdiag(bicup.data.res.arima1)

bicup.data.res.arima.pred <- forecast(bicup.data.res.arima, h = 63*7)
bicup.data.res.arima.pred1 <- forecast(bicup.data.res.arima1, h = 63*7)

model3 = accuracy(bicup.data.res.arima.pred,bicup.valid.ts)
model31 = accuracy(bicup.data.res.arima.pred1,bicup.valid.ts)

#fitARIMA <- arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")

bicup.data.res.arima.forecast <- forecast(bicup.data.res.arima, h = 63*10)

plot(bicup.data.res.arima.forecast$mean, main = "ARIMA's Forecast", xlim= c(2,25), xlab = "time", ylab = "Demand", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("blue", "green", "red"))

#Auto ARIMA model
bicup.data.arima <- auto.arima(bicup.train.ts)
bicup.data.arima.pred <- forecast(bicup.data.arima, h = 63*7)

summary(bicup.data.arima)
tsdiag(bicup.data.arima)

model31 = accuracy(bicup.data.arima.pred,bicup.valid.ts)

bicup.data.arima.forecast <- forecast(bicup.data.arima, h = 63*10)

plot(bicup.data.arima.forecast$mean, main = "AUTO ARIMA's Forecast", xlim= c(2,25), xlab = "time", ylab = "Demand", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("blue", "green", "red"))

#SARIMA model
library(fpp2)
install.packages("tseries")
library(urca)

ndiffs(bicup.train.ts)
nsdiffs(bicup.train.ts)

bicup.data.sarima=arima(bicup.train.ts,order=c(0,1,1),seasonal=list(order=c(1,1,0),period=63))
bicup.data.res.sarima.pred <- forecast(bicup.data.sarima, h = 63*7)

summary(bicup.data.sarima)
model4 = accuracy(bicup.data.res.sarima.pred,bicup.valid.ts)

bicup.data.res.sarima.forecast = forecast(bicup.data.sarima, h=63*10)

plot(bicup.data.res.sarima.forecast$mean, main = "Seasonal ARIMA's Forecast", xlim= c(2,25), xlab = "time", ylab = "Demand", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("blue", "green", "red"))

#auto Sarima
bicup.data<-read_excel("bicup2006.xls")
data = data.frame(bicup.data)
data$DATE=as.Date(data$DATE)
data$TIME=format(data$TIME, format = "%H:%M")
data$DATETIME <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y-%m-%d %H:%M")

df= data.frame(data$DEMAND)
df1 <- data.frame(head(df, n = 1058))
# select remaining rows for second dataframe
df2 <- data.frame(tail(df, n = 265))

bicup.train.ts = ts(df1, frequency = 63)
bicup.valid.ts = ts(df2, frequency = 63)
bicup.data.sarima=arima(bicup.train.ts,order=c(1,1,2),seasonal=list(order=c(1,2,2),period=63))

mod= auto.arima(bicup.train.ts)
acf(mod)

f_values=forecast(mod, h = 265)
f_values1 = ts(f_values$mean,frequency = 63)
accuracy(f_values1,bicup.valid.ts)

index <- c(1059:1323)
my_df <- data.frame(index,f_values1)

index1 = c(1:1058)
my_df1 = data.frame(index1,df1$data.DEMAND)

my_df2= data.frame(index,df2$data.DEMAND)

modforecast = forecast(mod,h=63*10)

plot(modforecast$mean, main = "Seasonal AUTO ARIMA's Forecast", xlim= c(2,25), xlab = "time", ylab = "Demand", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("blue", "green", "red"))
  
plot(my_df1$index1,my_df1$df1.data.DEMAND, col="red",type="l", xlim = c(700, 1300), main = "Forecast Graph", xlab ="time", ylab = "Demand")
lines(my_df$index,my_df$f_values1, col="green")
lines(my_df2$index,my_df2$df2.data.DEMAND, col = "blue")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("red","blue", "green"))

length(my_df1$df1.data.DEMAND)

#ENSEMBLE MODEL-SALMA 

bicup.data <- read_excel("bicup2006.xls")
df <- data.frame(bicup.data$DEMAND)

bicup.data<-read_excel("bicup2006.xls")
bicup.data.ts <- ts(bicup.data$DEMAND, start= c(1,1), frequency = 63)
nValid = 63*7                       
nTraining = length(bicup.data.ts)-nValid
bicup.train.ts = window(bicup.data.ts, end = c(1,nTraining))
bicup.valid.ts = window(bicup.data.ts, start = c(1,nTraining+1))

# Split the data into training and validation sets
#train <- df[1:1058,]
#valid <- df[1059:1323,]

# Fit ARIMA and SARIMA models
arima_fit <- auto.arima(bicup.train.ts, seasonal = TRUE)
sarima_fit <- Arima(bicup.train.ts, order = c(1,1,2), seasonal = list(order = c(2,1,1), period = 63))

# Forecast with the ARIMA and SARIMA models
arima_fcst <- forecast(arima_fit, h = 63*10)
sarima_fcst <- forecast(sarima_fit, h = 63*10)

# Ensemble the forecasts
ensemble_fcst <- (arima_fcst$mean + sarima_fcst$mean) / 2

# Compute accuracy measures for the ensemble forecast
accuracy(ensemble_fcst, valid)
summary(ensemble_fcst)
ensemble_forecast <- (forecast::meanf((arima_fcst$mean), (sarima_fcst$mean),h=63*10))
accuracy(ensemble_forecast)
ensemble.forecast <- (bicup.data.res.arima.forecast$mean + bicup.data.arima.forecast$mean + bicup.data.res.sarima.forecast$mean)/3

plot(ensemble.forecast, main = "Ensemble Forecast", xlim= c(2,25), xlab = "time", ylab = "Demand", col= "red")
lines(bicup.train.ts, col = "blue")
lines(bicup.valid.ts, col = "green")


#models separated data for weekends & weekdays by Urvish.
df = data.frame(read_excel("bicup2006.xls"))
df$DATE=as.Date(df$DATE)
df$TIME=format(df$TIME, format = "%H:%M")
df$DATETIME = as.POSIXct(paste(df$DATE, df$TIME), format = "%Y-%m-%d %H:%M")

df = subset(df, select = -c(DATE,TIME))
library(lubridate)
library(dplyr)
df$day <- wday(df$DATETIME, label=TRUE)

# filter data based on weekdays and weekends using dplyr
weekday_data <- df %>% filter(day %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))
weekend_data <- df %>% filter(day %in% c("Sat", "Sun"))

# Visualize the demand over time weekdy 
ggplot(weekday_data, aes(x = DATETIME, y = DEMAND)) +
  geom_line() +
  labs(title = "Demand over Time", x = "Date and Time", y = "Demand")


weekday_data.ts=ts(weekday_data$DEMAND, frequency = 63)

df1 <- data.frame(head(weekday_data$DEMAND, n = 630))
# select remaining rows for second dataframe
df2 <- data.frame(tail(weekday_data$DEMAND, n = 189))

plot(decompose(weekday_data.ts))

weekday_data.train.ts = ts(df1, frequency = 63)
weekday_data.valid.ts = ts(df2, frequency = 63)
weekday_data.sarima=arima(weekday_data.ts,order=c(1,1,2),seasonal=list(order=c(1,2,2),period=63))

mod= auto.arima(weekday_data.train.ts)
summary(mod)
f_values=forecast(mod, h = 189)
f_values1 = ts(f_values$mean,frequency = 63)
accuracy(f_values1,weekday_data.valid.ts)

index <- c(631:819)
my_df <- data.frame(index,f_values1)


index1 = c(1:630)
my_df1 = data.frame(index1,df1$head.weekday_data.DEMAND..n...630.)

my_df2= data.frame(index,df2$tail.weekday_data.DEMAND..n...189.)
plot(my_df1$index1,my_df1$df1.head.weekday_data.DEMAND..n...630., col="red",type="l", xlim = c(100, 900), main = "Forecast Graph", xlab ="time", ylab = "Demand")
lines(my_df$index,my_df$f_values1, col="green")
lines(my_df2$index,my_df2$df2.tail.weekday_data.DEMAND..n...189., col = "blue")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("red","blue", "green"))


#whole_fit
whole_fit=  auto.arima(weekday_data.ts)

f_values=forecast(mod, h = 189)
f_values1 = ts(f_values$mean,frequency = 63)
accuracy(f_values1,weekday_data.valid.ts)

index <- c(946:1134)
my_df <- data.frame(index,f_values1)


index1 = c(1:945)
my_df1 = data.frame(index1,weekday_data$DEMAND)

plot(my_df1$index1,my_df1$weekday_data.DEMAND, col="red",type="l", xlim = c(100, 1200), main = "Forecast Graph", xlab ="time", ylab = "Demand")
lines(my_df$index,my_df$f_values1, col="green")
legend("topright", legend = c("Data", "Forecast"), 
       lty = c(1, 1, 2), col = c("red", "green"))



# Visualize the demand over time weekends 
ggplot(weekend_data, aes(x = DATETIME, y = DEMAND)) +
  geom_line() +
  labs(title = "Demand over Time", x = "Date and Time", y = "Demand")


weekend_data.ts=ts(weekend_data$DEMAND, frequency = 63)

df1 <- data.frame(head(weekend_data$DEMAND, n = 315))
# select remaining rows for second dataframe
df2 <- data.frame(tail(weekend_data$DEMAND, n = 63))

plot(decompose(weekend_data.ts))

weekend_data.train.ts = ts(df1, frequency = 63)
weekend_data.valid.ts = ts(df2, frequency = 63)

mod= auto.arima(weekend_data.ts)

f_values=forecast(mod, h = 63)
f_values1 = ts(f_values$mean,frequency = 63)
accuracy(f_values1,weekend_data.valid.ts)

index <- c(316:378)
my_df <- data.frame(index,f_values1)


index1 = c(1:315)
my_df1 = data.frame(index1,df1$head.weekend_data.DEMAND..n...315.)

my_df2= data.frame(index,df2$tail.weekend_data.DEMAND..n...63.)
plot(my_df1$index1,my_df1$df1.head.weekend_data.DEMAND..n...315., col="red",type="l", xlim = c(100, 900), main = "Forecast Graph", xlab ="time", ylab = "Demand")
lines(my_df$index,my_df$f_values1, col="green")
lines(my_df2$index,my_df2$df2.tail.weekend_data.DEMAND..n...63., col = "blue")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("red","blue", "green"))


weekday_data.train.ts=ts(df1$head.weekend_data.DEMAND..n...315., start = c(1,1), frequency = 63)
weekend_data.valid.ts=ts(df2$tail.weekend_data.DEMAND..n...63., start = c(1,1), frequency = 63)
# Apply the STL algorithm to decompose the data
stl_fit <- stl(weekday_data.train.ts, s.window = "periodic")

# Plot the seasonal component
plot(stl_fit$time.series[, "seasonal"], type = "l")

f_values=forecast(stl_fit, h = 63)
f_values1 = ts(f_values$mean,frequency = 63)
accuracy(f_values1,weekend_data.valid.ts)

index <- c(316:378)
my_df <- data.frame(index,f_values1)


index1 = c(1:315)
my_df1 = data.frame(index1,df1$head.weekend_data.DEMAND..n...315.)

my_df2= data.frame(index,df2$tail.weekend_data.DEMAND..n...63.)
plot(my_df1$index1,my_df1$df1.head.weekend_data.DEMAND..n...315., col="red",type="l", xlim = c(100, 400), main = "Forecast Graph", xlab ="time", ylab = "Demand")
lines(my_df$index,my_df$f_values1, col="green")
lines(my_df2$index,my_df2$df2.tail.weekend_data.DEMAND..n...63., col = "blue")
legend("topright", legend = c("Train data", "Test data", "Forecast"), 
       lty = c(1, 1, 2), col = c("red","blue", "green"))
