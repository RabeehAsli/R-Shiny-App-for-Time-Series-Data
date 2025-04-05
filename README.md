# R-Shiny App for Time Series Analysis

# 📊 Interactive Time Series Analysis & Forecasting – R Shiny App

Welcome to your comprehensive toolkit for exploring, diagnosing, and modeling time series data in an intuitive, beginner-friendly way. Designed with learners and analysts in mind, this R Shiny app empowers users to dive deep into their time-dependent datasets—from preprocessing to modeling, residual diagnostics, and forecasting.

---

## 🔍 Key Features

### 📁 Upload Your Own Data

- Easily upload a CSV file with a date/time column and one or more numeric value columns.

### 🔬 Exploratory Analysis

- View Original Time Series
- Decompose into Trend, Seasonal, and Irregular Components

### 🪪 Statistical Testing

- *Stationarity Checks*: KPSS, ADF
- *Seasonality Detection*: HEGY Approximation
- ACF & PACF plots to visualize autocorrelations

### ➔ Differencing Engine

- Regular & Seasonal Differencing
- Visualize ACF/PACF before and after transformation
- Get auto-suggestions for optimal differencing order

### ⌐ Model Selection & Comparison

- Manually specify ARIMA/SARIMA parameters
- Or use Auto ARIMA for automatic selection
- View AIC/BIC and get model recommendations based on performance

### 📊 Residual Diagnostics

- Test residuals for normality (Shapiro/KS test)
- Check for autocorrelation (Ljung-Box)
- Assess variance stability (Breusch-Pagan Test)
- Get a verdict on residual quality

### 📈 Forecasting

- Forecast future values using Holt-Winters
- Choose forecast horizon and visualize predictions

### 🎨 User-Friendly UI

- Custom-themed interface with clean layout and responsive elements
- Tab-based navigation for a step-by-step analytical workflow

---

## 🚀 Getting Started

### 🛠 Dependencies

Install all required packages with:

r
install.packages(c("shiny", "ggplot2", "forecast", "tseries",
                   "zoo", "seastests", "rugarch",
                   "moments", "lmtest"))


### ▶ Run the App

Load the app in your R environment or RStudio:

r
library(shiny)
runApp("path/to/your/app/folder")


Or open app.R and hit *Run App*.

### 📄 CSV Format Required

- One date/time column (e.g., YYYY-MM, DD/MM/YYYY, Jan-2022)
- At least one numeric column to analyze

---

## 📚 Tech Stack

- *UI/Frontend*: R Shiny
- *Time Series Decomposition & Forecasting*: forecast, tseries
- *Statistical Testing*: lmtest, moments, seastests
- *Modeling*: ARIMA, SARIMA
- *Data Handling*: zoo, dplyr

---

## 🤝 Contributions Welcome

Want to enhance the UI, add models, or squash bugs?

- Fork the repo
- Submit pull requests or feature ideas
- Help grow this open-source learning tool!

---

## 💡 Built With Passion

Crafted by an R enthusiast who loves time series and believes in hands-on learning. This app makes complex concepts interactive and digestible.

---

## 🔗 Let’s Connect

📬 LinkedIn: [RABEEH ASLI VILLAN]\
📁 GitHub: [RabeehAsli]
