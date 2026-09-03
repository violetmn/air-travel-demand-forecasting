# ============================================================
# Exploratory Data Analysis
# International Air Travel Demand Forecasting
# ============================================================

# Load required libraries
library(ggplot2)
library(ggfortify)
library(forecast)
library(tseries)

# ------------------------------------------------------------
# 1. Load Data
# ------------------------------------------------------------

# AirPassengers is a built-in R dataset containing
# monthly international airline passenger totals from 1949–1960.
data("AirPassengers")

air_passengers <- AirPassengers

# Inspect the time series
print(air_passengers)
start(air_passengers)
end(air_passengers)
frequency(air_passengers)

# ------------------------------------------------------------
# 2. Plot Original Time Series
# ------------------------------------------------------------

autoplot(air_passengers) +
  labs(
    title = "Monthly International Airline Passengers",
    subtitle = "1949–1960",
    x = "Year",
    y = "Passengers (thousands)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 3. Examine Autocorrelation
# ------------------------------------------------------------

# Autocorrelation Function
acf(
  air_passengers,
  main = "ACF of Monthly Airline Passengers"
)

# Partial Autocorrelation Function
pacf(
  air_passengers,
  main = "PACF of Monthly Airline Passengers"
)

# ------------------------------------------------------------
# 4. Classical Time-Series Decomposition
# ------------------------------------------------------------

decomposition <- decompose(air_passengers)

autoplot(decomposition) +
  labs(
    title = "Classical Decomposition of Airline Passenger Demand"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 5. Basic Summary
# ------------------------------------------------------------

summary(air_passengers)
