# Lab 13 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  tseries,
  trend,
  forecast
)



# -------------------------------------------------------------------------


?Nile
class(Nile)

Nile
plot(Nile)



# Structure ---------------------------------------------------------------


AirPassengers

# Decompose into seasonal, trend, residual/random components
# Moving average to de-trend
# Average over each time period to get seasonal figure
# Random component is what is left over
# Multiplicative for when seasonal trend changes over time
AirPassengers_dec <- decompose(AirPassengers, type= "multiplicative")
plot(AirPassengers_dec)



# Types -------------------------------------------------------------------


# p, d, q = AR order, degree of differencing, moving average order


## White Noise - random, stationary, no trend, no change in variance, no AR
wn <- arima.sim(n = 100, list(order = c(0, 0, 0)))
plot(wn)
acf(wn)
pacf(wn)
# ACF shows no serial correlation

# White noise has normal distribution of values
hist(wn)
shapiro.test(wn)


## Random Walk - function of lag 1 + noise
rw <- arima.sim(n = 100, list(order = c(0, 1, 0)))
ggtsdisplay(rw)
# Function to show all three plots at once

# If we difference (de-trend) it, it becomes white noise
plot(diff(rw))
shapiro.test(diff(rw))
acf(diff(rw))

# Test for a trend
mk.test(rw)
# null: no trend


## AR1
ar1 <- arima.sim(n = 100, list(order = c(1, 0, 0), ar = 0.90))
plot(ar1)
acf(ar1)
pacf(ar1)
# PACF drops off after 1


## AR2
ar2 <- arima.sim(n = 100, list(order = c(2,0,0), ar = c(0.4, 0.4)))
plot(ar2)
acf(ar2)
pacf(ar2)
# PACF drops off after 2


## Moving Average
ma3 <- arima.sim(n = 100, list(order = c(0,0,3), ma = c(0.3, 0.3, 0.3)))
plot(ma3)
acf(ma3)
pacf(ma3)


## Identify parameters of model
(fit <- auto.arima(Nile))

# Evaluate fit through residuals
checkresiduals(fit)
# null: good fit



# Testing -----------------------------------------------------------------


## Trends and stationarity
# Stationary
kpss.test(AirPassengers)
# null: stationary

# Level stationary process
kpss.test(AirPassengers, null = 'Level')
# null: level

# Both
kpss.test(AirPassengers, null = 'Trend')


## Unit root process
adf.test(AirPassengers)
# null: unit root
# if significant,

nsdiffs(AirPassengers)
# seasonal



# ARIMA -------------------------------------------------------------------


arima1 <- auto.arima(AirPassengers)
arima1
summary(arima1)
checkresiduals(arima1)

arima2 <- auto.arima(log(AirPassengers), seasonal = TRUE)
arima2
summary(arima2)
checkresiduals(arima2)

# Get coefficients and p values
coeftest(arima2)
# positive shock of 1 reduces log of passengers by 0.4

# Forecast next year
pred <- forecast(arima2, h = 12)
pred
autoplot(pred)

