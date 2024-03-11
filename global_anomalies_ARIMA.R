# from ChaptGPT
# Load necessary libraries
library(forecast)
temps <- read.csv(file = "global_surface_anomalies.csv", # full file name
                  skip=3, # skip these lines because they contain metadata.
                  header = TRUE # Tell R that the first row is column namaes
) # last parenthesis

# Read the historical data into R

# Create a time series object
ts_data <- ts(temps$deg_C, start = c(1880), frequency = 1)

# Fit an ARIMA model to the data
arima_model <- auto.arima(ts_data, seasonal = FALSE, stepwise=FALSE, approximation=FALSE)

# Forecast future temperature anomalies
future_forecast <- forecast(arima_model, h=26)
plot(future_forecast)

# Extract the predicted values
predicted_anomalies <- future_forecast$mean

# Create a data frame with future years and projected anomalies
future_years <- 2024:2050
predictions <- data.frame(year = future_years, deg_C = predicted_anomalies)

# Print the projected temperature anomalies
print(predictions)


