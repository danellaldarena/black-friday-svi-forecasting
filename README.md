# Modeling and Forecasting Black Friday Search Volume Index

## Overview
This project analyzes and forecasts Black Friday search interest in Indonesia using monthly Google Trends Search Volume Index (SVI) data. The study explores time series patterns, including long-term trends, annual seasonality, and calendar effects associated with Black Friday.

Several time series models are developed and compared to identify an appropriate forecasting approach that captures both deterministic patterns and stochastic behavior in the data.

## Objectives
- Analyze the trend and seasonal patterns of Black Friday search interest in Indonesia.
- Examine the impact of calendar variations on search volume.
- Compare the performance of different time series forecasting models.
- Evaluate model accuracy and residual diagnostics.
- Forecast future Black Friday search interest using the selected model.

## Dataset
The dataset consists of monthly Google Trends SVI data for the keyword **"Black Friday"** in Indonesia, covering January 2015 to December 2025, with 132 observations.

Google Trends SVI uses a relative scale from 0 to 100, where 100 represents the highest search interest within the selected period.

## Methodology

### 1. Data Exploration
The dataset is examined to identify its main characteristics, including:
- Long-term trend
- Annual seasonal patterns
- Calendar-related variations
- Extreme search interest spikes during November

### 2. Feature Construction
The explanatory variables are designed to capture the temporal structure of the data:
- **Trend:** Represents long-term changes in search interest.
- **Monthly Seasonal Variables:** Capture recurring monthly patterns.
- **Fourier Terms:** Represent smooth periodic seasonal patterns.
- **Black Friday Calendar Effect:** Represents the recurring search spike associated with Black Friday.

### 3. Model Development
Eight model specifications are evaluated across three approaches:

- **ARIMAX Models:** Combine regression components with ARIMA errors.
- **Time Series Regression (TSR):** Models trend and seasonal patterns using regression.
- **Hybrid TSR-ARMA Model:** Combines deterministic regression components with stochastic error modeling.

### 4. Model Evaluation
The data is divided chronologically into:
- **Training Set:** January 2015 – December 2024.
- **Testing Set:** January 2025 – December 2025.

Model performance is evaluated using:
- Root Mean Squared Error (RMSE)
- Mean Absolute Percentage Error (MAPE)
- Symmetric Mean Absolute Percentage Error (sMAPE)

Residual adequacy is also examined using Autocorrelation Function (ACF) plots and the Ljung–Box test.

### 5. Forecasting
The selected model is used to forecast monthly Black Friday search interest from January to December 2026. A 95% prediction interval is included to represent forecast uncertainty.

## Results
The analysis indicates that Black Friday search interest exhibits strong annual seasonality, with recurring peaks in November, alongside long-term trend changes.

Models that incorporate seasonal and calendar-related components generally represent the observed patterns better than a trend-only regression model.

The **hybrid TSR model with ARMA errors (M8)** is selected as the final model based on its test performance and residual diagnostics. Its Ljung–Box test p-value is approximately 0.0549, slightly above the 0.05 significance level, indicating that the residuals can be considered close to white noise under the study's evaluation criteria.

The forecasts suggest that elevated search interest will continue to occur around Black Friday periods. However, extreme search spikes remain difficult to predict precisely, and forecasts should be interpreted with appropriate consideration of uncertainty.

## Project Scope
This project focuses on modeling and forecasting Google Trends search interest rather than actual transaction volumes, sales revenue, or consumer spending. The SVI represents relative search interest and should not be interpreted as the absolute number of searches.

## Authors
- Danella Citra Aldarena
- Maria Immaculata Dyah Hita Widowati
- Ruth Wahyu Sedjati Priyambodo
- Nicholas Brian Ariella

Department of Statistics  
Institut Teknologi Sepuluh Nopember (ITS)  
Surabaya, Indonesia
