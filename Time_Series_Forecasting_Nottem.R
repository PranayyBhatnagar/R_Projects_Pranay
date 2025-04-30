
# Load required libraries
library(ggplot2)
library(forecast)
library(tseries)

# Load dataset
data("nottem")  # Monthly air temperatures at Nottingham

# Convert to data frame
df <- data.frame(
  Date = as.Date(time(nottem)),
  Temperature = as.numeric(nottem)
)

# Visualize the data
ggplot(df, aes(x = Date, y = Temperature)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Monthly Average Temperature (Nottingham)", x = "Year", y = "Temperature (°F)") +
  theme_minimal()

# Convert to time series object
ts_data <- ts(df$Temperature, start = c(1920, 1), frequency = 12)

# Check for stationarity
adf_result <- adf.test(ts_data)
if (adf_result$p.value > 0.05) {
  ts_data_diff <- diff(ts_data)
  cat("Data was non-stationary. Differencing applied.\n")
} else {
  ts_data_diff <- ts_data
  cat("Data is stationary. No differencing applied.\n")
}

# Fit ARIMA and ETS models
arima_model <- auto.arima(ts_data)
ets_model <- ets(ts_data)

# Forecast for next 24 months
forecast_horizon <- 24
forecast_arima <- forecast(arima_model, h = forecast_horizon)
forecast_ets <- forecast(ets_model, h = forecast_horizon)

# Plot forecasts
autoplot(forecast_arima) +
  labs(title = "Forecast using ARIMA", x = "Year", y = "Temperature (°F)") +
  theme_minimal()

autoplot(forecast_ets) +
  labs(title = "Forecast using ETS", x = "Year", y = "Temperature (°F)") +
  theme_minimal()

# Split into train-test for model evaluation
train_data <- window(ts_data, end = c(1938, 12))
test_data <- window(ts_data, start = c(1939, 1))

# Re-fit models on training data
arima_train_model <- auto.arima(train_data)
ets_train_model <- ets(train_data)

# Forecast test data
arima_forecast_test <- forecast(arima_train_model, h = length(test_data))
ets_forecast_test <- forecast(ets_train_model, h = length(test_data))

# Define error metrics
calculate_metrics <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  list(MAE = mae, RMSE = rmse)
}

# Evaluate performance
metrics_arima <- calculate_metrics(test_data, arima_forecast_test$mean)
metrics_ets <- calculate_metrics(test_data, ets_forecast_test$mean)

# Display results
cat("\nModel Performance Metrics:\n")
cat("ARIMA -> MAE:", round(metrics_arima$MAE, 2), " | RMSE:", round(metrics_arima$RMSE, 2), "\n")
cat("ETS   -> MAE:", round(metrics_ets$MAE, 2), " | RMSE:", round(metrics_ets$RMSE, 2), "\n")
