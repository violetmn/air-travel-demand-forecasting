# ============================================================
# Forecast Evaluation
# International Air Travel Demand Forecasting
# ============================================================

library(forecast)

# ------------------------------------------------------------
# 1. Recreate selected harmonic regression residuals
# ------------------------------------------------------------

data("AirPassengers")

log_ap_ts <- ts(
  log(AirPassengers),
  start = c(1949, 1),
  frequency = 12
)

time_vals_log_k2 <- time(log_ap_ts)

harm_model_log_k2 <- lm(
  log_ap_ts ~ time_vals_log_k2 + harmonic(log_ap_ts, 2)
)

resid_log_k2 <- residuals(harm_model_log_k2)

resid_log_k2_ts <- ts(
  resid_log_k2,
  start = c(1949, 1),
  frequency = 12
)

# ============================================================
# 2. Train / test split
# ============================================================

# Train: 1949–1958
# Test: 1959–1960 (24 months)

train_ts <- window(
  resid_log_k2_ts,
  end = c(1958, 12)
)

test_ts <- window(
  resid_log_k2_ts,
  start = c(1959, 1)
)

# ============================================================
# 3. SARIMA(2,1,1)(0,1,1)[12]
# ============================================================

sarima_211 <- Arima(
  train_ts,
  order = c(2, 1, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

pred_211 <- forecast(
  sarima_211,
  h = 24
)

msfe_211 <- mean(
  (test_ts - pred_211$mean)^2
)

mafe_211 <- mean(
  abs(test_ts - pred_211$mean)
)

# ============================================================
# 4. SARIMA(1,1,2)(0,1,1)[12]
# ============================================================

sarima_112 <- Arima(
  train_ts,
  order = c(1, 1, 2),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

pred_112 <- forecast(
  sarima_112,
  h = 24
)

msfe_112 <- mean(
  (test_ts - pred_112$mean)^2
)

mafe_112 <- mean(
  abs(test_ts - pred_112$mean)
)

# ============================================================
# 5. SARIMA(0,1,1)(0,1,1)[12]
# ============================================================

sarima_011 <- Arima(
  train_ts,
  order = c(0, 1, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

pred_011 <- forecast(
  sarima_011,
  h = 24
)

msfe_011 <- mean(
  (test_ts - pred_011$mean)^2
)

mafe_011 <- mean(
  abs(test_ts - pred_011$mean)
)

# ============================================================
# 6. Test-set comparison
# ============================================================

test_results <- data.frame(

  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "SARIMA(2,1,1)(0,1,1)[12]",
    "SARIMA(1,1,2)(0,1,1)[12]"
  ),

  MSFE = c(
    msfe_011,
    msfe_211,
    msfe_112
  ),

  MAFE = c(
    mafe_011,
    mafe_211,
    mafe_112
  )
)

print(test_results)

# ============================================================
# 7. Rolling one-step-ahead forecast evaluation
# ============================================================

rolling_sarima <- function(
  y,
  order,
  seasonal,
  period = 12,
  h = 1,
  window_size = 120
) {

  s <- length(y) - window_size + 1

  ehat <- numeric(s)

  for (j in 1:s) {

    train_sub <- y[
      j:(j + window_size - h - 1)
    ]

    test_sub <- y[
      j + window_size - h
    ]

    model <- Arima(
      train_sub,
      order = order,
      seasonal = list(
        order = seasonal,
        period = period
      )
    )

    forecast_val <- forecast(
      model,
      h = h
    )$mean[h]

    ehat[j] <- test_sub - forecast_val
  }

  return(ehat)
}

# ------------------------------------------------------------
# Rolling forecast errors
# ------------------------------------------------------------

err_011 <- rolling_sarima(
  resid_log_k2_ts,
  c(0, 1, 1),
  c(0, 1, 1)
)

err_211 <- rolling_sarima(
  resid_log_k2_ts,
  c(2, 1, 1),
  c(0, 1, 1)
)

err_112 <- rolling_sarima(
  resid_log_k2_ts,
  c(1, 1, 2),
  c(0, 1, 1)
)

# ============================================================
# 8. Rolling forecast metrics
# ============================================================

msfe_roll_011 <- mean(err_011^2)
mafe_roll_011 <- mean(abs(err_011))

msfe_roll_211 <- mean(err_211^2)
mafe_roll_211 <- mean(abs(err_211))

msfe_roll_112 <- mean(err_112^2)
mafe_roll_112 <- mean(abs(err_112))

# ============================================================
# 9. Full model comparison
# ============================================================

forecast_results <- data.frame(

  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "SARIMA(2,1,1)(0,1,1)[12]",
    "SARIMA(1,1,2)(0,1,1)[12]"
  ),

  Test_MSFE = c(
    msfe_011,
    msfe_211,
    msfe_112
  ),

  Test_MAFE = c(
    mafe_011,
    mafe_211,
    mafe_112
  ),

  Rolling_MSFE = c(
    msfe_roll_011,
    msfe_roll_211,
    msfe_roll_112
  ),

  Rolling_MAFE = c(
    mafe_roll_011,
    mafe_roll_211,
    mafe_roll_112
  )
)

print(forecast_results)

# ============================================================
# 10. Forecast plot for selected model
# ============================================================

plot(
  pred_211,
  main = "24-Month Forecast: SARIMA(2,1,1)(0,1,1)[12]",
  xlab = "Year",
  ylab = "Harmonic Regression Residuals"
)

lines(
  test_ts,
  lwd = 2
)

# ============================================================
# Conclusion
# ============================================================

# SARIMA(2,1,1)(0,1,1)[12] achieved the lowest holdout
# MSFE and MAFE and was selected as the preferred model
# for the final forecasting pipeline.
