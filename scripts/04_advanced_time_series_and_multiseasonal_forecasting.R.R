

# ASSIGNMENT-6 

#_PART-1: Forecasting Highway Traffic 2 Volume______________________________________________________


# I am Loading necessary libraries
library(forecast)
library(ggplot2)
library(tidyverse)
library(lubridate)

# I am Reading the data
traffic <- read.csv("C:/Users/saipr/Downloads/traffic_MS.csv")

# Check ing the structure
str(traffic)
summary(traffic)

#-------------------------------------------------------------------------------------------------------------------------

#2. Preparaing proper timeseries dataset. Plot the t datafile and try to understand its behavior.

colnames(traffic)
head(traffic, 3)

traffic <- read.csv("C:/Users/saipr/Downloads/traffic_MS.csv", header = TRUE)
library(lubridate)

# Doing Proper parsing
traffic$Timestamp <- mdy_hm(traffic$Timestamp)

# I am Checking that it's parsed
head(traffic$Timestamp)

# Below i am Removing rows where timestamp parsing failed, to get better forecast. 
traffic <- traffic[!is.na(traffic$Timestamp), ]

library(forecast)

# Create multiseasonal time series object
traffic_ts <- msts(traffic$Traffic_Volume,
                   seasonal.periods = c(24, 24 * 7))  # hourly, daily and weekly seasonality

# Plot to inspect
autoplot(traffic_ts) +
  ggtitle("Hourly Traffic Volume") +
  xlab("Time Index") +
  ylab("Volume")

# Note: The above plot shows strong daily spikes (hourly cycles), with variation over the weeks — this confirms both daily and weekly seasonality are present.


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3. Timeseries exploration

# I am Decomposing the multi-seasonal time series
decomp <- mstl(traffic_ts)

# Below I am Plotting the decomposition
autoplot(decomp) + 
  ggtitle("Multi-Seasonal Decomposition of Traffic Volume")

#Here in the above plot, we see 
# Trend : Slightly increasing then declining. 
# Daily seasonality: Sharp peaks (rush hours).
# Weekly seasonality: repeating waves (Week day variation).
#Remainder has random flutuations or noise. 

#---------------------------------------------------------------------------------------------------------------------------

# 4. Building models
# Partition timeseries. Eight weeks Training. Two weeks validation.
# Build forecasting models with STL+ETS (stlm() in R), tbats , and ets().
# Run the models on validation. Comparing the forecast results.

#Note: 
# Training = First 8 weeks = 8 × 7 × 24 = 1344 hours
# Validation = Final 2 weeks = 336 hours
# Total = 1680 observations


train_ts <- traffic_ts[1:1344]
valid_ts <- traffic_ts[1345:1680]


length(train_ts)  # Should be 1344
length(valid_ts)  # Should be 336



# Convert all to ts objects for autoplot. 

# Here, Training (8 weeks = 56 days)
train_ts_plot <- ts(as.numeric(train_ts), frequency = 24, start = c(1, 1))

# Here,  Validation starts at Day 57
valid_ts_plot <- ts(as.numeric(valid_ts), frequency = 24, start = c(57, 1))

# Forecasts start at same time as validation
stlm_fcast_ts  <- ts(as.numeric(stlm_forecast$mean), frequency = 24, start = c(57, 1))
tbats_fcast_ts <- ts(as.numeric(tbats_forecast$mean), frequency = 24, start = c(57, 1))
ets_fcast_ts   <- ts(as.numeric(ets_forecast$mean), frequency = 24, start = c(57, 1))

# I am Plot all together
autoplot(train_ts_plot, series = "Training") +
  autolayer(valid_ts_plot, series = "Validation") +
  autolayer(stlm_fcast_ts, series = "STL+ETS Forecast") +
  autolayer(tbats_fcast_ts, series = "TBATS Forecast") +
  autolayer(ets_fcast_ts, series = "ETS Forecast") +
  ggtitle("Forecast Comparison on Validation Period") +
  xlab("Time Index") + ylab("Traffic Volume") +
  guides(colour = guide_legend(title = "Series"))



# Now Comparing Accuracy to find the best MODEL 

accuracy(stlm_forecast, valid_ts)
accuracy(tbats_forecast, valid_ts)
accuracy(ets_forecast, valid_ts)


# From the above we can say TBATS is best model because of low RMSE, MAE, MAPE values. 


#-------------------------------------------------------------------------------------------------------

# 5. Below i am Forecasting one week into the future with the best model.

# Refit TBATS model on full data
final_model <- tbats(traffic_ts)

# Below i am Forecasting next 7 days (168 hours)
future_forecast <- forecast(final_model, h = 168)

# Plotting the forecast
autoplot(future_forecast) +
  ggtitle("7-Day Forecast of Traffic Volume (TBATS)") +
  xlab("Time (Hours)") + ylab("Forecasted Traffic Volume")


#--------------------------------------------------------------------------------------------------------------

# 6. Preparing a plot of
# Training timeseries
# Validation timeseries
# Validation forecasts with all three models’ forecast.
# Future forecast

# I am doing Actual Training and Validation as separate series below
training_series   <- ts(c(train_ts, rep(NA, 336 + 168)), frequency = 24, start = c(1, 1))
validation_series <- ts(c(rep(NA, 1344), valid_ts, rep(NA, 168)), frequency = 24, start = c(1, 1))

# Forecasts
stlm_series  <- ts(c(rep(NA, 1344), stlm_forecast$mean, rep(NA, 168)), frequency = 24, start = c(1, 1))
tbats_series <- ts(c(rep(NA, 1344), tbats_forecast$mean, future_forecast$mean), frequency = 24, start = c(1, 1))
ets_series   <- ts(c(rep(NA, 1344), ets_forecast$mean, rep(NA, 168)), frequency = 24, start = c(1, 1))

# Below is a Combined Plot
autoplot(training_series, series = "Training") +
  autolayer(validation_series, series = "Validation") +
  autolayer(stlm_series, series = "STL+ETS Forecast") +
  autolayer(tbats_series, series = "TBATS Forecast (Validation + Future)") +
  autolayer(ets_series, series = "ETS Forecast") +
  ggtitle("Training, Validation, Forecasts, and Future Forecast") +
  xlab("Time (Days)") + ylab("Traffic Volume") +
  guides(colour = guide_legend(title = "Series"))

#-------------------------------------------------------------------------------------------------------------

#Conclusion: Based on Insights i think, 

# The best way to staff toll booths is to increase workers during peak hours and decrease them during periods of low traffic.

# It is possible to plan maintenance tasks for the weekends or early mornings to reduce traffic interruption.

# Identifying periods of high traffic and allocating resources appropriately helps improve emergency response strategy.


#---------------------------------------------------------------------------------------------------------------------------------


#______PART-2: Cooking Oil Sales______________________________________________________________________________________________


# Load libraries
library(tidyverse)
library(lubridate)
library(forecast)

# Load the dataset
oil <- read.csv("C:/Users/saipr/Downloads/Cooking Oil Price.csv")

# View structure
str(oil)
head(oil)

#------------------------------------------------------------------------------------

# I am Converting the Date column from character to Date
oil$Date <- as.Date(oil$Date)

# I am Creating Trend variable (1 to 120 for Jan 2000 to Dec 2009)
oil$Trend <- 1:nrow(oil)

# I am Creating month as factor for seasonality
oil$MonthFactor <- as.factor(month(oil$Date))

#------------------------------------------------------------------------------------

# forecasting the sales in January and February 2008, used data upto 2007 only. Used log (Sales). 

# Below i am Training data up to Dec 2007
oil_2008_train <- filter(oil, Date <= as.Date("2007-12-01"))

# Below i am Fitting linear regression model on log(Sales)
model_2008 <- lm(log(Sales) ~ Trend + MonthFactor, data = oil_2008_train)

# I am Building future data for Jan & Feb 2008
future_2008 <- data.frame(
  Trend = c(nrow(oil_2008_train) + 1, nrow(oil_2008_train) + 2),
  MonthFactor = factor(c(1, 2), levels = levels(oil$MonthFactor))
)

# Forecast and back-transform
log_forecast_2008 <- predict(model_2008, newdata = future_2008)
forecast_2008 <- exp(log_forecast_2008)

#----------------------------------------------------------------------------------------


# forecasting the sales in January and February 2010, did not use data beyond 2009 for this.
# Used log (Sales).

# Below i am training data up to Dec 2009
oil_2010_train <- filter(oil, Date <= as.Date("2009-12-01"))

# Fitting the model
model_2010 <- lm(log(Sales) ~ Trend + MonthFactor, data = oil_2010_train)

# I am Buildling future data for Jan & Feb 2010
future_2010 <- data.frame(
  Trend = c(nrow(oil_2010_train) + 1, nrow(oil_2010_train) + 2),
  MonthFactor = factor(c(1, 2), levels = levels(oil$MonthFactor))
)

# Forecast and back-transform
log_forecast_2010 <- predict(model_2010, newdata = future_2010)
forecast_2010 <- exp(log_forecast_2010)


#---------------------------------------------------------------------------------------------

# Below i am displaying the forecasts. 

cat("Forecast for Jan & Feb 2008:\n")
print(forecast_2008)

cat("\nForecast for Jan & Feb 2010:\n")
print(forecast_2010)


# Conclusion: 

# Jan 2008: 216.76
# February 2008: 242.0046
# January 2010: 380.20
# February 2010: 421.73

# The projections show a general upward trend in sales over the course of the decade, driven by both time progression and monthly seasonal influences.

#------------------------------------------------------------------------------------------------

#-----------------------------_ THE END_-----------------------------------------------