#ASSIGNMENT-3 
# PART_B

#---------------------------------------------------------


#a) Reading the data from the file. 

# Load required package
library(zoo)

# Read the RDS file (assuming it's in your working directory)
data <- readRDS("C:/Users/saipr/Downloads/australian Eating Habit.rda")

# View the time series
print(data)

#------------------------------------------------------------------------------------------

# B) Extracting the time series dataset features

# The Start date
start(data) 

# The End date
end(data)  

# The Seasonal frequency]
frequency(data) 

# The Total number of observations
length(data)    


plot(data, main = "Australian Eating Habit - Time Series Plot", ylab = "Value", xlab = "Time")

# i) The plot is showing trend and seasonal pattern hence it is not stationary.
# ii) There are 426 observations

#-----------------------------------------------------------------------------------------------

# c.	Here i am Converting the time series into data-frame with two columns, Date, and time series values. The first column holding date, starting with the start date and ending with the end date. 
# Also i am creating a plot of the data-frame. 

df <- data.frame(
  Date = as.yearmon(time(data)),
  time_series_values = as.numeric(data)
)

# Plot
plot(df$Date, df$time_series_values, type = "l", main = "Eating Habit Over Time", xlab = "Date", ylab = "Time Series Values")

#--------------------------------------------------------------------------------------------------

# d) Below i am Transforming the newly made data-frame into time series using ts().
# I am also Plotting the time series. 

timeseries_data <- ts(df$time_series_values, start = start(data), frequency = frequency(data))
plot(timeseries_data, main = "Time Series Plot", ylab = "Time Series Values")

# From the plot, i can say that 
# Trend is increasing over time strongly.


#--------------------------------------------------------------------------------------------------

# e) Below i am decomposing the time series 

decomp <- decompose(timeseries_data)
plot(decomp)
#i) 
# By decomposition of Additive time series i see observed, trend, seasonal, random plots. 
# Seasonal seems repeating every 12 months. 
# In random it seems like there are fluctuations after removing trend and seasonality. 

#ii) 
# I can say there is similar trend is matching upward movement from original/ observed series. 



#----------------------------------------------------------------------------------------------------

# f) Below i am Partitioning the dataset into train and validation. 
# I am Making a reasonable choice for partitions’ size.

Valid <- 24
Train <- length(timeseries_data) - Valid

train.ts <- window(timeseries_data, end = c(time(timeseries_data)[Train]))
validation.ts <- window(timeseries_data, start = c(time(timeseries_data)[Train + 1]))

#-------------------------------------------------------------------------------------------------

library(forecast)

# g.	Use SES, Holts, and  Holts-Winter algorithms to build a forecasting model. 

# SES
ses.model <- ses(train.ts, h = Valid, alpha = 0.2)

# Holts
holts.model <- holt(train.ts, h = Valid, alpha = 0.2, beta = 0.1)

# Holts-Winter
hw.model <- HoltWinters(train.ts, alpha = 0.2, beta = 0.1, gamma = 0.1)
hw.forecast <- forecast(hw.model, h = Valid)


# Above I am Applying the model on validation now on all three models above.
# And also using alpha, beta, gamma constants above for the algorithms.

#---------------------------------------------------------------------------------------------------

# H)  I am Plotting all models of validation in one plot below

plot(validation.ts, main = "Validation Set Forecasts")
lines(ses.model$mean, col = "deepskyblue", lty = 2)
lines(holts.model$mean, col = "green", lty = 2)
lines(hw.forecast$mean, col = "firebrick", lty = 2)
legend("topleft", legend = c("Actual", "SES", "Holt", "Holt-Winters"), 
       col = c("black", "deepskyblue", "green", "firebrick"), lty = 1:2)


# By seeing the plot i can say, the holts-winters seems to be following the original series well and best than any other model. 

#------------------------------------------------------------------------------------------------------------------------------------------------


# i) I am Plotting the training, actual validation, and forecast validation with Holts-Winter in one plot.

plot(timeseries_data, main = "The Full Series with Holt-Winters Forecast")
lines(hw.forecast$fitted, col = "deepskyblue")
lines(validation.ts, col = "black")
lines(hw.forecast$mean, col = "firebrick")


#-------------------------------------------------------------------------------------------------------------------------------------------------


# j) I am Plotting the residuals below. 

resid_hw <- residuals(hw.forecast)
plot(resid_hw, main = "Residuals")

# i) By viewing the residuals i can say that,it is stationary.
# it is fluctuating around zero, but no specific pattern or trend or seasonality.
# Can see very slight increase in variance over time, but it is not that much. 


#------------------------------------------------------------------------------------------------------------------------------------------------------

# k) Below i am Calculating the accuracy of these models. 

accuracy(ses.model, validation.ts)
accuracy(holts.model, validation.ts)
accuracy(hw.forecast, validation.ts)


# By viewing the accuracy metrics of the aboves models, i can say that Holt's winters model is best. 
# Because it has lower values of MAPE and RMSE values, Theil's U also low means it shows good in forecasting. 


#--------------------------------------------------------------------------------------------------------------------------------------------------------

# l.	Below i Forecast for 15 months in the future with the right model of Holt's-winters.

# i.	 Also i Plotted the forecast and interpretted the result through a visualization. 

final.forecast <- forecast(hw.model, h = 15)

# Plot forecast
plot(final.forecast, main = "15-Month Future Forecast of Eating Habit in Australia",
     ylab = "Time Series Values", xlab = "Time")

# This plot forecasts next 15 months, capturing all trends and seasonality well of past patterns. 
# Through visualization, i see trend seems to be high, increasing over time.
# As well as seasonality following every 12 months also appears. 
# The model captures effectively and giving good results to us, it is working good. 
# Even if we compare the plot with original it matches well proving our forecast more accurate.

#---------------------------------------------------------------------------------------------------------

# THE END 



