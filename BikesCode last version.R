# Downloading packages
library(forecast)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
# Downloading data
bikes <- read.csv("dublinbikes.csv")
geo <- read.csv("Districts.csv")
weather <- read.csv("Historic weather.csv")
summary(bikes)
summary(geo)
summary(weather)

#Removing not important variables and joining data
bikes <- bikes %>% 
  separate(TIME, c('Date', 'Time'), " ") %>%
  left_join(weather, by = c("Date"="date")) %>%
  left_join(geo, by = c("STATION.ID"="original_Number")) %>%
  select(STATION.ID:Time, NAME:AVAILABLE.BIKES, maxtp:rain, district) 

#Choose one station for sample forecasting (impossible to forecast all stations at the same time)
bikes_station <- bikes %>%
  filter(STATION.ID == "2") #example, we can change it to the ID of another station
bikes_station

#Adding date format
bikes_station <- bikes_station %>%
  mutate(Dates = as.Date(Date, format = '%Y-%m-%d')) %>%
  mutate(Times = as.ITime(Time)) %>%
  mutate( Date_Time = as.POSIXct(paste(Dates, Times), format="%Y-%m-%d %H:%M")) %>%
  select(-Date, -Dates, -Times)
summary(bikes_station)

#Removing excess timestamps to change frequency of records to hourly 
bikes_station <- bikes_station %>%
  separate(Time, sep = ":", into = c("Hours", "Minutes", "Seconds")) %>%
  #Change these columns into numeric vectors
  mutate_at(c("Hours", "Minutes", "Seconds"), as.numeric) %>%
  filter(Minutes == 0) %>%
  select(-Hours,-Minutes,-Seconds)

View(bikes_station)

#filling dates
datestimes <- data.frame(DateTime = seq(as.POSIXct("2021-10-01"),
                                as.POSIXct("2022-01-01"), 
                                by=(60*60)))
#Missing values
st <- max(bikes_station$STATION.ID)
name <- max(bikes_station$NAME)
bs <-max(bikes_station$BIKE.STANDS)
ABS <- round(mean((bikes_station$AVAILABLE.BIKE.STANDS)))
ab <- round(mean((bikes_station$AVAILABLE.BIKES)))
mt <- mean(bikes_station$maxtp)
mit <- mean(bikes_station$mintp)
rai <-mean(bikes_station$rain)
distr <- max(bikes_station$district)
#Replace missing values
fullbikes <- datestimes %>%
  left_join(bikes_station, by=c("DateTime"="Date_Time")) %>%
  replace_na(list(STATION.ID = st, NAME = name, BIKE.STANDS = bs,
                  maxtp = mt, mintp = mit, rain = rai, district=distr))
fullbikes <- fullbikes %>% 
  mutate(AVAILABLE.BIKE.STANDS = ifelse(is.na(AVAILABLE.BIKE.STANDS), ABS, AVAILABLE.BIKE.STANDS)) %>%
  mutate(AVAILABLE.BIKES = ifelse(is.na(AVAILABLE.BIKES), ab, AVAILABLE.BIKES)) 

View(fullbikes)
summary(fullbikes)

bikes.ts<-ts(fullbikes$AVAILABLE.BIKES)

plot(bikes.ts)
summary(bikes.ts)
bikes.ts

#There is 24 observation for 1 day so frequency is 24
#2 months training 1 month validation

df <- fullbikes[c('DateTime', 'AVAILABLE.BIKES')]
df

train.ts <- window(bikes.ts, start=c(1), end=c(1464))
train.ts
autoplot(train.ts)

daily <- ts(df$AVAILABLE.BIKES, start = c(1,1), frequency = 24)
autoplot(daily, facets = FALSE) + xlab('DAYS') + ylab('Available Bikes') + ggtitle('Time Series for the daily available bikes')

weekly<- ts(df$AVAILABLE.BIKES, start = c(1,1), frequency = 168)
autoplot(weekly, facets = FALSE) + xlab('Weeks') + ylab('Available Bikes')+ ggtitle('Time Series for the weekly available bikes')
weekly
decompose_weekly_add <- decompose(weekly, 'additive')
autoplot(decompose_weekly_add) + xlab('Weeks')

#There is an unstable trend and seasonality and noise

#splitting data

nValid<-744
#31*24=744
nTrain <- length(weekly) - nValid
train.ts <- window(weekly, start = c(1, 1), end = c(1, nTrain))
valid.ts <- window(weekly, start = c(1, nTrain+1), end = c(1, nTrain+nValid))
plot(train.ts)
plot(valid.ts)


#Mean
aver.pred <- meanf(train.ts, h=nValid)
autoplot(aver.pred, ylab = "Available Bikes")
accuracy(aver.pred,valid.ts)
#MPE and MAPE is inf as data contains zero

#Seasonal Naive
snaive.pred <- snaive(train.ts, h = nValid)
autoplot(snaive.pred,ylab = "Available Bikes")
accuracy(snaive.pred,valid.ts)

#Linear Model with seasonality
train.lm1 <- tslm(train.ts ~ season)
train.lm.pred1 <- forecast(train.lm1, h = nValid)
autoplot(train.lm.pred1,ylab = "Available Bikes")
accuracy(train.lm.pred1,valid.ts)
#When comparing with the snaive model, linear model works better as it has a lower RMSE and MAE

#Linear model with trend and seasonality
train.lm2 <- tslm(train.ts ~ trend+season)
train.lm.pred2 <- forecast(train.lm2, h = nValid)
autoplot(train.lm.pred2,ylab = "Available Bikes")
accuracy(train.lm.pred2,valid.ts)
#With the Trend component we get lower RMSE.

#Holtz-Winters model
bikes.hw <- ets(train.ts, model = "ZZZ")
hw.pred <- forecast(bikes.hw,h=nValid)
autoplot(hw.pred,ylab = "Available Bikes")
accuracy(hw.pred, valid.ts)
#Error metrics are high for validation set, holtz-winters model does not work properly

#Linear model with trend and seasonality has lowest error metrics
test.pred <- forecast(train.lm2, h = nValid)
plot(train.ts, ylab = "Available Bikes", xlab = "Week", bty = "l", xlim = c(0,14), main = "",xaxt = "n")
#We have the plot of train
lines(test.pred$mean, lwd = 2, col = "yellow", lty = 1)
axis(1, at = c(0:14))
lines(valid.ts, col = "grey20", lty = 3)
lines(c(9.7, 9.7), c(0, 20), col='red')
#than we are creating lines to separate the train and validation set
text(5, 20, "Training", size = 2, col = 'darkgreen')
text(11, 20,"Validation", size = 2, col = 'darkgreen')






