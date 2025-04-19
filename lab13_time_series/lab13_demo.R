# Lab 13 Demo


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  tseries,
  trend,
  forecast,
  lmtest,
  fpp2
)



# Structure ---------------------------------------------------------------


?AirPassengers
AirPassengers
start(AirPassengers)
end(AirPassengers)
length(AirPassengers)
frequency(AirPassengers)
deltat(AirPassengers)

plot(AirPassengers)

# Decompose into seasonal, trend, residual/random components
# Moving average to de-trend
# Average over each time period to get seasonal figure
# Random component is what is left over
# Multiplicative for when seasonal trend changes over time
air_decompose <- decompose(AirPassengers, type = "multiplicative")
plot(air_decompose)



# Types -------------------------------------------------------------------


# (p, d, q)
# p: auto-regressive order
# d: differences needed to remove trend
# q: moving average order


## White Noise - random, stationary, no trend, no change in variance, no AR
wn <- arima.sim(n = 100, list(order = c(0, 0, 0)))
plot(wn)
forecast::Acf(wn)  # auto-correlation function
forecast::Pacf(wn) # partial auto-correlation function
# ACF shows no serial correlation

# White noise has normal distribution of values
hist(wn)
shapiro.test(wn)


## Random Walk - function of lag 1 + noise
set.seed(42)
rw <- arima.sim(n = 100, list(order = c(0, 1, 0)))
forecast::ggtsdisplay(rw)
# Function to show all three plots at once
# Stationary or non-stationary?

# Test for a trend
trend::mk.test(rw)
# null: no trend

# If we difference (de-trend) it, it becomes white noise
diff <- diff(rw)
plot(diff)
shapiro.test(diff)
forecast::Acf(diff)
forecast::Pacf(diff)


## AR1
ar1 <- arima.sim(n = 100, list(order = c(1, 0, 0), ar = 0.90))
ggtsdisplay(ar1)
# PACF drops off after 1
# Stationary or not?


## AR2
ar2 <- arima.sim(n = 100, list(order = c(2, 0, 0), ar = c(0.4, 0.4)))
forecast::ggtsdisplay(ar2)
# PACF drops off after 2


## Moving Average - based on last q error terms
ma1 <- arima.sim(n = 100, list(order = c(0, 0, 2), ma = c(0.8, 0.8)))
forecast::ggtsdisplay(ma1)
# ACF bounces between positive and negative



# Testing -----------------------------------------------------------------


## Trends
# Level process
tseries::kpss.test(AirPassengers, null = 'Level')
# null: level

# Trend
tseries::kpss.test(AirPassengers, null = 'Trend')
# null: trend

# Shows we have a non-level trend


## Unit root process
tseries::adf.test(AirPassengers)
# null: unit root (similar to stationary)

# How many differences to remove trends
forecast::nsdiffs(AirPassengers)
# 1 diff to remove season



# ARIMA -------------------------------------------------------------------


# Need stationary time series for ARIMA models
# Great details here: https://otexts.com/fpp2/arima-r.html

## Fit automatic arima model
arima1 <- forecast::auto.arima(AirPassengers)

# See how residuals look
forecast::checkresiduals(arima1)


## Adjust for heteroskedasticity
arima2 <- auto.arima(log(AirPassengers))
forecast::checkresiduals(arima2)

# Get coefficients and p values
lmtest::coeftest(arima2)
# positive shock of 1 reduces log of passengers by 0.4


## Forecast next year
pred <- forecast::forecast(arima2, h = 12)
pred
forecast::autoplot(pred)



# Dynamic -----------------------------------------------------------------


?uschange
uschange
forecast::autoplot(uschange[, 1:2], facets = TRUE)

fit <- auto.arima(uschange[, "Consumption"], xreg = uschange[, "Income"])
summary(fit)
forecast::checkresiduals(fit)
lmtest::coeftest(fit)



# Resources ---------------------------------------------------------------


# Forecasting: Principles and Practice - https://otexts.com/fpp2/
# DataCamp: Time Series Analysis in R -
#   https://app.datacamp.com/learn/courses/time-series-analysis-in-r
# DataCamp: ARIMA models in R -
#   https://app.datacamp.com/learn/courses/arima-models-in-r


