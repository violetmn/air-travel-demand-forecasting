# ============================================================
# Harmonic Regression Residual Diagnostics
# International Air Travel Demand Forecasting
# ============================================================
library(forecast)
library(tseries)
library(randtests)

# ------------------------------------------------------------
# 1. Prepare transformed series
# ------------------------------------------------------------

data("AirPassengers")

log_ap_ts <- ts(
  log(AirPassengers),
  start = c(1949, 1),
  frequency = 12
)

lambda <- BoxCox.lambda(log_ap_ts)

ts_boxcox <- BoxCox(log_ap_ts, lambda)

ts_boxcox <- ts(
  ts_boxcox,
  start = c(1949, 1),
  frequency = 12
)

# ------------------------------------------------------------
# 2. Refit k = 2 harmonic models
# ------------------------------------------------------------

time_vals_log_k2 <- time(log_ap_ts)

harm_model_log_k2 <- lm(
  log_ap_ts ~ time_vals_log_k2 + harmonic(log_ap_ts, 2)
)

resid_log_k2 <- residuals(harm_model_log_k2)


time_vals_bc_k2 <- time(ts_boxcox)

harm_model_bc_k2 <- lm(
  ts_boxcox ~ time_vals_bc_k2 + harmonic(ts_boxcox, 2)
)

resid_bc_k2 <- residuals(harm_model_bc_k2)

# ============================================================
# 3. Residual Autocorrelation
# ============================================================

# Log-transformed model
acf(
  resid_log_k2,
  main = "ACF of Harmonic Regression Residuals (Log, k = 2)"
)

pacf(
  resid_log_k2,
  main = "PACF of Harmonic Regression Residuals (Log, k = 2)"
)

# Box-Cox model
acf(
  resid_bc_k2,
  main = "ACF of Harmonic Regression Residuals (Box-Cox, k = 2)"
)

pacf(
  resid_bc_k2,
  main = "PACF of Harmonic Regression Residuals (Box-Cox, k = 2)"
)

# ============================================================
# 4. Stationarity Tests
# ============================================================

# Augmented Dickey-Fuller tests
adf_log <- adf.test(resid_log_k2)
adf_boxcox <- adf.test(resid_bc_k2)

print(adf_log)
print(adf_boxcox)

# ============================================================
# 5. Residual Independence / Whiteness
# ============================================================

ljung_log <- Box.test(
  resid_log_k2,
  lag = 20,
  type = "Ljung-Box"
)

ljung_boxcox <- Box.test(
  resid_bc_k2,
  lag = 20,
  type = "Ljung-Box"
)

print(ljung_log)
print(ljung_boxcox)

# ============================================================
# 6. Normality Tests
# ============================================================

shapiro_log <- shapiro.test(resid_log_k2)
shapiro_boxcox <- shapiro.test(resid_bc_k2)

print(shapiro_log)
print(shapiro_boxcox)

# ------------------------------------------------------------
# Q-Q plots
# ------------------------------------------------------------

par(mfrow = c(1, 2))

qqnorm(
  resid_log_k2,
  main = "Q-Q Plot: Log Model Residuals"
)

qqline(resid_log_k2)

qqnorm(
  resid_bc_k2,
  main = "Q-Q Plot: Box-Cox Model Residuals"
)

qqline(resid_bc_k2)

par(mfrow = c(1, 1))

# ============================================================
# 7. Runs Tests for Randomness
# ============================================================

runs_log <- runs(resid_log_k2)
runs_boxcox <- runs(resid_bc_k2)

print(runs_log)
print(runs_boxcox)



# ------------------------------------------------------------
# Conclusion
# ------------------------------------------------------------

# The k = 2 harmonic models produce approximately normal residuals,
# but the ADF and Ljung-Box tests indicate that the residuals are
# not stationary and still contain significant autocorrelation.
# The next step is therefore to difference the selected log-model
# residuals and fit ARMA models.
