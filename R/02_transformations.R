# ============================================================
# Variance Stabilization
# International Air Travel Demand Forecasting
# ============================================================

library(ggplot2)
library(ggfortify)
library(forecast)

data("AirPassengers")

air_passengers <- AirPassengers

# ------------------------------------------------------------
# 1. Log Transformation
# ------------------------------------------------------------

log_ap_ts <- ts(
  log(air_passengers),
  start = c(1949, 1),
  frequency = 12
)

autoplot(log_ap_ts) +
  ggtitle("Log-Transformed AirPassengers") +
  ylab("Log(Passengers)") +
  xlab("Year") +
  theme_minimal()

# ------------------------------------------------------------
# 2. Box-Cox Transformation
# ------------------------------------------------------------

# Estimate lambda from the log-transformed series,
# consistent with the later modeling workflow
lambda <- BoxCox.lambda(log_ap_ts)

ts_boxcox <- BoxCox(log_ap_ts, lambda)

ts_boxcox <- ts(
  ts_boxcox,
  start = c(1949, 1),
  frequency = 12
)

autoplot(ts_boxcox) +
  ggtitle(
    paste0(
      "Box-Cox Transformed Series (lambda = ",
      round(lambda, 3),
      ")"
    )
  ) +
  ylab("Transformed Passengers") +
  xlab("Year") +
  theme_minimal()

# Print selected lambda
print(lambda)
