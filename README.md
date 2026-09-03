# air-travel-demand-forecasting
Time-series forecasting of international airline passenger demand using harmonic regression, SARIMA, rolling validation, and statistical model comparison in R.
# International Air Travel Demand Forecasting

Time-series forecasting of monthly international airline passenger demand using harmonic regression and SARIMA modeling in R.

## Project Overview

This project analyzes monthly international airline passenger data from 1949 to 1960 and develops a forecasting pipeline to model long-term growth, seasonality, and residual autocorrelation.

The final approach combines:

- Log transformation for variance stabilization
- Harmonic regression with two harmonics to model trend and seasonality
- SARIMA(2,1,1)(0,1,1)[12] modeling of harmonic-regression residuals
- Holdout and rolling forecast evaluation
- Final 12-month forecast for 1961

  
## Methodology

The forecasting pipeline was developed in several stages:

1. **Exploratory Data Analysis**  
   Examined long-term trend, seasonality, autocorrelation, and changing variance in the monthly passenger series.

2. **Variance Stabilization**  
   Applied logarithmic and Box-Cox transformations to address increasing variance over time.

3. **Harmonic Regression**  
   Compared models using 1, 2, and 4 harmonics. The log-transformed model with two harmonics provided the best balance of model fit and interpretability.

4. **Residual Diagnostics**  
   Evaluated the harmonic regression residuals using ACF/PACF plots, Augmented Dickey-Fuller, Ljung-Box, Shapiro-Wilk, Q-Q plots, and runs tests.

5. **ARMA and SARIMA Modeling**  
   Investigated ARMA models for remaining autocorrelation before moving to seasonal ARIMA models to capture both seasonal and non-seasonal dependence.

6. **Forecast Evaluation**  
   Compared finalist SARIMA models using a 24-month holdout set, MSFE, MAFE, and rolling one-step-ahead forecasts.

7. **Final Forecast**  
   Combined the harmonic regression forecast with SARIMA forecasts of the residual component to generate a 12-month forecast for 1961.
