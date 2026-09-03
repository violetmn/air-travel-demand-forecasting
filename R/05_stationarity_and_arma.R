# ============================================================
# Stationarity and ARMA Modeling
# International Air Travel Demand Forecasting
# ============================================================

library(forecast)
library(tseries)
library(randtests)
library(TSA)

# ------------------------------------------------------------
# 1. Recreate the selected harmonic regression model
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
# 2. Difference the harmonic regression residuals
# ============================================================

diff_resid <- diff(resid_log_k2)

# Examine autocorrelation after differencing
acf(
  diff_resid,
  main = "ACF of Differenced Harmonic Residuals"
)

pacf(
  diff_resid,
  main = "PACF of Differenced Harmonic Residuals"
)

# ============================================================
# 3. Test stationarity and residual structure
# ============================================================

# Augmented Dickey-Fuller test
adf_diff <- adf.test(diff_resid)
print(adf_diff)

# KPSS test
kpss_diff <- kpss.test(diff_resid)
print(kpss_diff)

# Ljung-Box test for autocorrelation
ljung_diff <- Box.test(
  diff_resid,
  lag = 20,
  type = "Ljung-Box"
)

print(ljung_diff)

# Runs test for randomness
runs_diff <- runs(diff_resid)
print(runs_diff)

# ============================================================
# 4. Identify ARMA candidates using EACF
# ============================================================

eacf(diff_resid)

# ============================================================
# 5. Fit candidate ARMA models
# ============================================================

models <- list(

  arma_013 = arima(
    diff_resid,
    order = c(0, 0, 3)
  ),

  arma_113 = arima(
    diff_resid,
    order = c(1, 0, 3)
  ),

  arma_213 = arima(
    diff_resid,
    order = c(2, 0, 3)
  ),

  arma_212 = arima(
    diff_resid,
    order = c(2, 0, 2)
  )

)

# ============================================================
# 6. Visual diagnostics
# ============================================================

tsdiag(models$arma_013)
tsdiag(models$arma_113)
tsdiag(models$arma_213)
tsdiag(models$arma_212)

# ============================================================
# 7. Extract model residuals
# ============================================================

res_013 <- residuals(models$arma_013)
res_113 <- residuals(models$arma_113)
res_213 <- residuals(models$arma_213)
res_212 <- residuals(models$arma_212)

# ============================================================
# 8. ARMA(0,3) diagnostics
# ============================================================

aic_013 <- AIC(models$arma_013)
bic_013 <- BIC(models$arma_013)

lb_013 <- Box.test(
  res_013,
  lag = 20,
  type = "Ljung-Box"
)

runs_013 <- runs.test(res_013)

shapiro_013 <- shapiro.test(res_013)

# ============================================================
# 9. ARMA(1,3) diagnostics
# ============================================================

aic_113 <- AIC(models$arma_113)
bic_113 <- BIC(models$arma_113)

lb_113 <- Box.test(
  res_113,
  lag = 20,
  type = "Ljung-Box"
)

runs_113 <- runs.test(res_113)

shapiro_113 <- shapiro.test(res_113)

# ============================================================
# 10. ARMA(2,3) diagnostics
# ============================================================

aic_213 <- AIC(models$arma_213)
bic_213 <- BIC(models$arma_213)

lb_213 <- Box.test(
  res_213,
  lag = 20,
  type = "Ljung-Box"
)

runs_213 <- runs.test(res_213)

shapiro_213 <- shapiro.test(res_213)

# ============================================================
# 11. ARMA(2,2) diagnostics
# ============================================================

aic_212 <- AIC(models$arma_212)
bic_212 <- BIC(models$arma_212)

lb_212 <- Box.test(
  res_212,
  lag = 20,
  type = "Ljung-Box"
)

runs_212 <- runs.test(res_212)

shapiro_212 <- shapiro.test(res_212)

# ============================================================
# 12. Compare model diagnostics
# ============================================================

arma_results <- data.frame(

  Model = c(
    "ARMA(0,3)",
    "ARMA(1,3)",
    "ARMA(2,3)",
    "ARMA(2,2)"
  ),

  AIC = c(
    aic_013,
    aic_113,
    aic_213,
    aic_212
  ),

  BIC = c(
    bic_013,
    bic_113,
    bic_213,
    bic_212
  ),

  Ljung_Box_p = c(
    lb_013$p.value,
    lb_113$p.value,
    lb_213$p.value,
    lb_212$p.value
  ),

  Runs_Test_p = c(
    runs_013$p.value,
    runs_113$p.value,
    runs_213$p.value,
    runs_212$p.value
  ),

  Shapiro_Wilk_p = c(
    shapiro_013$p.value,
    shapiro_113$p.value,
    shapiro_213$p.value,
    shapiro_212$p.value
  )
)

print(arma_results)
