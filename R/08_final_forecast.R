# ============================================================
# Final Forecasting Pipeline
# International Air Travel Demand Forecasting
# ============================================================

library(forecast)

# ------------------------------------------------------------
# 1. Load full dataset
# ------------------------------------------------------------

data("AirPassengers")

log_ap_ts <- ts(
  log(AirPassengers),
  start = c(1949, 1),
  frequency = 12
)

# ============================================================
# 2. Fit final harmonic regression model
# ============================================================

time_vals_log <- time(log_ap_ts)

harmonics <- harmonic(
  ts(log_ap_ts, frequency = 12),
  m = 2
)

colnames(harmonics) <- c(
  "cos1",
  "cos2",
  "sin1",
  "sin2"
)

df <- data.frame(
  log_ap_ts = as.numeric(log_ap_ts),
  time_vals_log = time_vals_log,
  harmonics
)

harm_model_log_k2 <- lm(
  log_ap_ts ~
    time_vals_log +
    cos1 +
    cos2 +
    sin1 +
    sin2,
  data = df
)

# Extract residuals from harmonic regression
resid_log_k2 <- residuals(
  harm_model_log_k2
)

resid_log_k2_ts <- ts(
  resid_log_k2,
  start = c(1949, 1),
  frequency = 12
)

# ============================================================
# 3. Fit final SARIMA model to harmonic residuals
# ============================================================

final_model <- Arima(
  resid_log_k2_ts,
  order = c(2, 1, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

# Forecast residual component for 1961
resid_forecast <- forecast(
  final_model,
  h = 12
)

# ============================================================
# 4. Generate harmonic component for 1961
# ============================================================

future_time_vals_log <-
  time_vals_log[length(time_vals_log)] +
  (1:12) / 12

future_ts <- ts(
  rep(0, 12),
  start = c(1961, 1),
  frequency = 12
)

future_harm <- harmonic(
  future_ts,
  m = 2
)

colnames(future_harm) <- c(
  "cos1",
  "cos2",
  "sin1",
  "sin2"
)

newdata <- data.frame(
  time_vals_log = future_time_vals_log,
  future_harm
)

future_trend <- predict(
  harm_model_log_k2,
  newdata = newdata
)

# ============================================================
# 5. Combine harmonic and SARIMA forecasts
# ============================================================

log_forecast_full_model <-
  future_trend +
  resid_forecast$mean

log_forecast_ts <- ts(
  log_forecast_full_model,
  start = c(1961, 1),
  frequency = 12
)

# ============================================================
# 6. Plot final log-scale forecast
# ============================================================

plot(
  log_ap_ts,
  xlim = c(1949, 1962),
  ylim = range(
    log_ap_ts,
    log_forecast_ts
  ),
  main = "Final Forecast — Harmonic Regression + SARIMA",
  ylab = "Log(Passengers)",
  xlab = "Year"
)

lines(
  log_forecast_ts,
  lwd = 2
)

points(
  log_forecast_ts,
  pch = 19
)

legend(
  "topleft",
  legend = c(
    "Observed",
    "Forecast"
  ),
  lty = 1,
  lwd = 2
)

# ============================================================
# 7. Back-transform forecast to passenger scale
# ============================================================

passenger_forecast <- exp(
  log_forecast_ts
)

passenger_forecast_ts <- ts(
  passenger_forecast,
  start = c(1961, 1),
  frequency = 12
)

# Print forecast values
print(passenger_forecast_ts)

# ============================================================
# 8. Plot forecast on original passenger scale
# ============================================================

plot(
  AirPassengers,
  xlim = c(1949, 1962),
  ylim = range(
    AirPassengers,
    passenger_forecast_ts
  ),
  main = "Forecast of International Airline Passenger Demand",
  ylab = "Passengers (thousands)",
  xlab = "Year"
)

lines(
  passenger_forecast_ts,
  lwd = 2
)

points(
  passenger_forecast_ts,
  pch = 19
)

legend(
  "topleft",
  legend = c(
    "Observed",
    "1961 Forecast"
  ),
  lty = 1,
  lwd = 2
)

# ============================================================
# Final Model
# ============================================================

# Final forecasting pipeline:
#
# Log transformation
#        +
# Harmonic regression (k = 2)
#        +
# SARIMA(2,1,1)(0,1,1)[12] on harmonic residuals
#
# The two forecast components are combined and then
# transformed back to the original passenger scale.
