# ============================================================
# Harmonic Regression Modeling
# International Air Travel Demand Forecasting
# ============================================================

library(forecast)
library(ggplot2)
library(ggfortify)

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

# ============================================================
# 2. LOG-TRANSFORMED SERIES
# ============================================================

# ------------------------------------------------------------
# k = 1
# ------------------------------------------------------------

time_vals_log_k1 <- time(log_ap_ts)

harm_model_log_k1 <- lm(
  log_ap_ts ~ time_vals_log_k1 + harmonic(log_ap_ts, 1)
)

fitted_log_k1 <- ts(
  fitted(harm_model_log_k1),
  start = start(log_ap_ts),
  frequency = frequency(log_ap_ts)
)

resid_log_k1 <- residuals(harm_model_log_k1)

plot(
  log_ap_ts,
  type = "l",
  lwd = 2,
  ylab = "Log(Passengers)",
  xlab = "Time",
  main = "Log(AirPassengers) with Harmonic Fit (k = 1)"
)

lines(
  fitted_log_k1,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual", "Fitted"),
  lty = 1,
  lwd = 2
)

plot(
  resid_log_k1,
  type = "l",
  main = "Residuals from Harmonic Regression (Log, k = 1)",
  ylab = "Residuals",
  xlab = "Time"
)

abline(
  h = 0,
  lty = "dashed"
)

# ------------------------------------------------------------
# k = 2
# ------------------------------------------------------------

time_vals_log_k2 <- time(log_ap_ts)

harm_model_log_k2 <- lm(
  log_ap_ts ~ time_vals_log_k2 + harmonic(log_ap_ts, 2)
)

fitted_log_k2 <- ts(
  fitted(harm_model_log_k2),
  start = start(log_ap_ts),
  frequency = frequency(log_ap_ts)
)

resid_log_k2 <- residuals(harm_model_log_k2)

plot(
  log_ap_ts,
  type = "l",
  lwd = 2,
  ylab = "Log(Passengers)",
  xlab = "Time",
  main = "Log(AirPassengers) with Harmonic Fit (k = 2)"
)

lines(
  fitted_log_k2,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual", "Fitted"),
  lty = 1,
  lwd = 2
)

plot(
  resid_log_k2,
  type = "l",
  main = "Residuals from Harmonic Regression (Log, k = 2)",
  ylab = "Residuals",
  xlab = "Time"
)

abline(
  h = 0,
  lty = "dashed"
)

# ------------------------------------------------------------
# k = 4
# ------------------------------------------------------------

time_vals_log_k4 <- time(log_ap_ts)

harm_model_log_k4 <- lm(
  log_ap_ts ~ time_vals_log_k4 + harmonic(log_ap_ts, 4)
)

fitted_log_k4 <- ts(
  fitted(harm_model_log_k4),
  start = start(log_ap_ts),
  frequency = frequency(log_ap_ts)
)

resid_log_k4 <- residuals(harm_model_log_k4)

plot(
  log_ap_ts,
  type = "l",
  lwd = 2,
  ylab = "Log(Passengers)",
  xlab = "Time",
  main = "Log(AirPassengers) with Harmonic Fit (k = 4)"
)

lines(
  fitted_log_k4,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual", "Fitted"),
  lty = 1,
  lwd = 2
)

plot(
  resid_log_k4,
  type = "l",
  main = "Residuals from Harmonic Regression (Log, k = 4)",
  ylab = "Residuals",
  xlab = "Time"
)

abline(
  h = 0,
  lty = "dashed"
)

# ============================================================
# 3. BOX-COX-TRANSFORMED SERIES
# ============================================================

# ------------------------------------------------------------
# k = 1
# ------------------------------------------------------------

time_vals_bc_k1 <- time(ts_boxcox)

harm_model_bc_k1 <- lm(
  ts_boxcox ~ time_vals_bc_k1 + harmonic(ts_boxcox, 1)
)

fitted_bc_k1 <- ts(
  fitted(harm_model_bc_k1),
  start = start(ts_boxcox),
  frequency = frequency(ts_boxcox)
)

resid_bc_k1 <- residuals(harm_model_bc_k1)

plot(
  ts_boxcox,
  type = "l",
  lwd = 2,
  ylab = paste0("Box-Cox (lambda = ", round(lambda, 3), ")"),
  xlab = "Time",
  main = "Harmonic Regression with Box-Cox (k = 1)"
)

lines(
  fitted_bc_k1,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual (Box-Cox)", "Fitted"),
  lty = 1,
  lwd = 2
)

plot(
  resid_bc_k1,
  type = "l",
  main = "Residuals from Harmonic Regression (Box-Cox, k = 1)",
  ylab = "Residuals",
  xlab = "Time"
)

abline(
  h = 0,
  lty = "dashed"
)

# ------------------------------------------------------------
# k = 2
# ------------------------------------------------------------

time_vals_bc_k2 <- time(ts_boxcox)

harm_model_bc_k2 <- lm(
  ts_boxcox ~ time_vals_bc_k2 + harmonic(ts_boxcox, 2)
)

fitted_bc_k2 <- ts(
  fitted(harm_model_bc_k2),
  start = start(ts_boxcox),
  frequency = frequency(ts_boxcox)
)

resid_bc_k2 <- residuals(harm_model_bc_k2)

plot(
  ts_boxcox,
  type = "l",
  lwd = 2,
  ylab = paste0("Box-Cox (lambda = ", round(lambda, 3), ")"),
  xlab = "Time",
  main = "Harmonic Regression with Box-Cox (k = 2)"
)

lines(
  fitted_bc_k2,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual (Box-Cox)", "Fitted"),
  lty = 1,
  lwd = 2
)

plot(
  resid_bc_k2,
  type = "l",
  main = "Residuals from Harmonic Regression (Box-Cox, k = 2)",
  ylab = "Residuals",
  xlab = "Time"
)

abline(
  h = 0,
  lty = "dashed"
)

# ------------------------------------------------------------
# k = 4
# ------------------------------------------------------------

time_vals_bc_k4 <- time(ts_boxcox)

harm_model_bc_k4 <- lm(
  ts_boxcox ~ time_vals_bc_k4 + harmonic(ts_boxcox, 4)
)

fitted_bc_k4 <- ts(
  fitted(harm_model_bc_k4),
  start = start(ts_boxcox),
  frequency = frequency(ts_boxcox)
)

resid_bc_k4 <- residuals(harm_model_bc_k4)

plot(
  ts_boxcox,
  type = "l",
  lwd = 2,
  ylab = paste0("Box-Cox (lambda = ", round(lambda, 3), ")"),
  xlab = "Time",
  main = "Harmonic Regression with Box-Cox (k = 4)"
)

lines(
  fitted_bc_k4,
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual (Box-Cox)", "Fitted"),
  lty = 1,
  lwd = 2
)

plot(
  resid_bc_k4,
  type = "l",
  main = "Residuals from Harmonic Regression (Box-Cox, k = 4)",
  ylab = "Residuals",
  xlab = "Time"
)

abline(
  h = 0,
  lty = "dashed"
)

# ============================================================
# 4. Compare model summaries
# ============================================================

summary(harm_model_log_k1)
summary(harm_model_log_k2)
summary(harm_model_log_k4)

summary(harm_model_bc_k1)
summary(harm_model_bc_k2)
summary(harm_model_bc_k4)
