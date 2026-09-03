# ============================================================
# SARIMA Modeling
# International Air Travel Demand Forecasting
# ============================================================

library(forecast)
library(tseries)
library(randtests)
library(TSA)

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

# ============================================================
# 2. Seasonal + non-seasonal differencing
# ============================================================

final_diff <- diff(
  diff(resid_log_k2),
  lag = 12
)

# Examine the transformed residual series
acf(
  final_diff,
  main = "ACF of Seasonally Differenced Residuals"
)

pacf(
  final_diff,
  main = "PACF of Seasonally Differenced Residuals"
)

# ============================================================
# 3. Stationarity and autocorrelation checks
# ============================================================

adf_final <- adf.test(final_diff)

ljung_final <- Box.test(
  final_diff,
  lag = 20,
  type = "Ljung-Box"
)

print(adf_final)
print(ljung_final)

# ============================================================
# 4. Identify SARIMA candidates using EACF
# ============================================================

eacf(final_diff)

# ============================================================
# 5. Fit candidate SARIMA models
# ============================================================

sarima_011_011 <- arima(
  resid_log_k2,
  order = c(0, 1, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

sarima_111_011 <- arima(
  resid_log_k2,
  order = c(1, 1, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

sarima_211_011 <- arima(
  resid_log_k2,
  order = c(2, 1, 1),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

sarima_112_011 <- arima(
  resid_log_k2,
  order = c(1, 1, 2),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

sarima_013_011 <- arima(
  resid_log_k2,
  order = c(0, 1, 3),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

sarima_113_011 <- arima(
  resid_log_k2,
  order = c(1, 1, 3),
  seasonal = list(
    order = c(0, 1, 1),
    period = 12
  )
)

# ============================================================
# 6. Compare AIC and BIC
# ============================================================

sarima_results <- data.frame(

  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "SARIMA(1,1,1)(0,1,1)[12]",
    "SARIMA(2,1,1)(0,1,1)[12]",
    "SARIMA(1,1,2)(0,1,1)[12]",
    "SARIMA(0,1,3)(0,1,1)[12]",
    "SARIMA(1,1,3)(0,1,1)[12]"
  ),

  AIC = c(
    AIC(sarima_011_011),
    AIC(sarima_111_011),
    AIC(sarima_211_011),
    AIC(sarima_112_011),
    AIC(sarima_013_011),
    AIC(sarima_113_011)
  ),

  BIC = c(
    BIC(sarima_011_011),
    BIC(sarima_111_011),
    BIC(sarima_211_011),
    BIC(sarima_112_011),
    BIC(sarima_013_011),
    BIC(sarima_113_011)
  )
)

print(sarima_results)

# ============================================================
# 7. Extract residuals
# ============================================================

res011 <- residuals(sarima_011_011)
res111 <- residuals(sarima_111_011)
res211 <- residuals(sarima_211_011)
res112 <- residuals(sarima_112_011)
res013 <- residuals(sarima_013_011)
res113 <- residuals(sarima_113_011)

# ============================================================
# 8. Residual diagnostics
# ============================================================

diagnose_sarima <- function(residuals) {

  list(

    Ljung_Box = Box.test(
      residuals,
      lag = 20,
      type = "Ljung-Box"
    ),

    Shapiro_Wilk = shapiro.test(
      residuals
    ),

    Runs_Test = runs(
      residuals
    )

  )
}

diag_011 <- diagnose_sarima(res011)
diag_111 <- diagnose_sarima(res111)
diag_211 <- diagnose_sarima(res211)
diag_112 <- diagnose_sarima(res112)
diag_013 <- diagnose_sarima(res013)
diag_113 <- diagnose_sarima(res113)

# ============================================================
# 9. Create diagnostic comparison table
# ============================================================

sarima_diagnostics <- data.frame(

  Model = c(
    "SARIMA(0,1,1)(0,1,1)[12]",
    "SARIMA(1,1,1)(0,1,1)[12]",
    "SARIMA(2,1,1)(0,1,1)[12]",
    "SARIMA(1,1,2)(0,1,1)[12]",
    "SARIMA(0,1,3)(0,1,1)[12]",
    "SARIMA(1,1,3)(0,1,1)[12]"
  ),

  Ljung_Box_p = c(
    diag_011$Ljung_Box$p.value,
    diag_111$Ljung_Box$p.value,
    diag_211$Ljung_Box$p.value,
    diag_112$Ljung_Box$p.value,
    diag_013$Ljung_Box$p.value,
    diag_113$Ljung_Box$p.value
  ),

  Shapiro_Wilk_p = c(
    diag_011$Shapiro_Wilk$p.value,
    diag_111$Shapiro_Wilk$p.value,
    diag_211$Shapiro_Wilk$p.value,
    diag_112$Shapiro_Wilk$p.value,
    diag_013$Shapiro_Wilk$p.value,
    diag_113$Shapiro_Wilk$p.value
  ),

  Runs_Test_p = c(
    diag_011$Runs_Test$pvalue,
    diag_111$Runs_Test$pvalue,
    diag_211$Runs_Test$pvalue,
    diag_112$Runs_Test$pvalue,
    diag_013$Runs_Test$pvalue,
    diag_113$Runs_Test$pvalue
  )
)

print(sarima_diagnostics)

# ============================================================
# 10. Detailed residual plots for strongest candidates
# ============================================================

# SARIMA(0,1,1)(0,1,1)[12]

par(mfrow = c(2, 2))

acf(
  res011,
  main = "ACF - SARIMA(0,1,1)"
)

pacf(
  res011,
  main = "PACF - SARIMA(0,1,1)"
)

qqnorm(
  res011,
  main = "Q-Q Plot - SARIMA(0,1,1)"
)

qqline(res011)

hist(
  res011,
  main = "Histogram - SARIMA(0,1,1)",
  xlab = "Residuals"
)

# SARIMA(2,1,1)(0,1,1)[12]

par(mfrow = c(2, 2))

acf(
  res211,
  main = "ACF - SARIMA(2,1,1)"
)

pacf(
  res211,
  main = "PACF - SARIMA(2,1,1)"
)

qqnorm(
  res211,
  main = "Q-Q Plot - SARIMA(2,1,1)"
)

qqline(res211)

hist(
  res211,
  main = "Histogram - SARIMA(2,1,1)",
  xlab = "Residuals"
)

# SARIMA(1,1,2)(0,1,1)[12]

par(mfrow = c(2, 2))

acf(
  res112,
  main = "ACF - SARIMA(1,1,2)"
)

pacf(
  res112,
  main = "PACF - SARIMA(1,1,2)"
)

qqnorm(
  res112,
  main = "Q-Q Plot - SARIMA(1,1,2)"
)

qqline(res112)

hist(
  res112,
  main = "Histogram - SARIMA(1,1,2)",
  xlab = "Residuals"
)

par(mfrow = c(1, 1))

# ============================================================
# 11. Final candidates
# ============================================================

# Based on model fit, coefficient significance, and residual
# diagnostics, the following three models were retained for
# forecast evaluation:
#
# SARIMA(0,1,1)(0,1,1)[12]
# SARIMA(2,1,1)(0,1,1)[12]
# SARIMA(1,1,2)(0,1,1)[12]
