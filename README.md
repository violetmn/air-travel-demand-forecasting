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
