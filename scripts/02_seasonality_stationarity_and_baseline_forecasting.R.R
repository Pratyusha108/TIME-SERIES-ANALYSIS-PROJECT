#Assignment-2 
#---------------------------------------------------------------------------

# a.	Using R, read the time series data using readRDS() in R and plot it for visualization. 

# I am loading the data
birth_data <- readRDS("C:/Users/saipr/Downloads/birth.rda")

# Seeing structure of the data
str(birth_data)

# Shows the first few rows by printing them
head(birth_data)


# Plotting the time series for visualization
plot(birth_data, main="US Birth Rate (1948-1979)", ylab="Births (in thousands)", xlab="Time", col="red")

#-----------------------------------------------------------------------------------------

# b.	As a comment in your R script answer the following:



# i.	Is this dataset stationary?

# Answer: The plot is not constant, it has variance and trend, hence it is not stationary. 
#(Observed both from plot and features as it shows seasonality)


# ii.	What is the seasonality frequency and the trend?

# The seasonality frequency is 12 months, as birthrate is repeating trend every 12 months and trend is also flucatuating with both upper and lower trend with change over time. 
# Seasonality = 12 (can be seen in features below too)
# trend = 0.9776599 (from features)


# iii.	What is the starting date and ending date of this time series? Note: You can extract these info by using tsfeatures() function.

#Using the below code to find start and end dates of this time series 
install.packages("tsfeatures")
library(tsfeatures)
features_in_data <- tsfeatures(birth_data)
print(features_in_data)

#From above using tsfeatures() function, i found out starting_date is 1948 and ending_date is 1979




#-------------------------------------------------------------------------------------------------------


# Question: c.	Convert the time series data into a data frame format. Add a new date column as the first column to your new data frame. This column should have the starting and ending dates of the observations. Save your new data frame as “USBirth.csv”. 

# Answer: 

# I am converting the time series data to a data frame format below, creating a new date column

birth_data_df <- data.frame(Date = seq.Date(from = as.Date("1948-01-01"), 
                                       by = "month", 
                                       length.out = length(birth_data)), 
                       Births = as.numeric(birth_data))

# To view the first few rows in dataframe
head(birth_data_df)

# I am Saving the data frame as "USBirth.csv"

write.csv(birth_data_df, file = "USBirth.csv", row.names = FALSE)


#-----------------------------------------------------------------------------------------------------------

# Question: d.	Create a time series of your modified datafile with your selected frequency.

# Answer: 

# Reading the csv file
birth_data_df <- read.csv("USBirth.csv")

# Since seasonality frequency = 12, lets us select frequency to be 12 because any other frequency can lead to miscalculations in seasonality.  

births_time_series <- ts(birth_data_df$Births, start = c(1948, 1), frequency = 12)

print(head(births_time_series))

# Plotting new time series 
plot(births_time_series, main="US Birth Rate (Monthly)", ylab="Births (in thousands)", xlab="Time", col="red")


#-------------------------------------------------------------------------------------------------------

# Question: e.	Partition the time series into a training partition and a validation partition. 
#You can choose 26 years for training and 5 years and one month for validation.  
# As a comment in your R code, explain is it better to have a larger validation? Why?

# Answer: 

# I am Partitioning the time series

training <- 26 * 12  
# Calculating the no. of months in the training period (26 years)

validation <- 5 * 12 + 1  
# Calculating the no. of months in the validation period (5 years + 1 month)

print(training)
print(validation)


# In textbook from chapter-3, window() was used to create training & validation partition, i am doing the same below 


training_partition <- window(births_time_series, start = c(1948, 1), end = c(1948, training))

validation_partition <- window(births_time_series, start = c(1948, training + 1), end = c(1948, training + validation))

# Below i am checking the length of each partition 

cat("Training Partiton Length:", length(training_partition), "\n")
cat("Validation Partition Length:", length(validation_partition), "\n")

# Below, I am plotting both training and Validation Partitions to visualize if done perfectly or not. 

plot(training_partition, main="Training Partition (26 Years)", ylab="Births", xlab="Time", col="red")
plot(validation_partition, main="Validation Partition (5 Years + 1 Month)", ylab="Births", xlab="Time", col="purple")

# It is done good



# No, it is not better to have large validation, even if we can attain most recent patterns and trends from it.
# Even it is better for long-term analysis, i think it is important that it doesn't effect the training partition making it less.
# Having large training data more cruicial i think. 




#------------------------------------------------------------------------------------------------------------------------------------


# Question: f.	Create Forecast validations in R  with naïve, snaive, trailing moving average. Use a window size of 5.

# Answer: 



# Loading the forecast package (suggested in textbook)

library(forecast)


# Below is Naïve forecast (most recent value)

naive <- naive(training_partition, h = length(validation_partition))


# Below is Seasonal Naïve forecast (Viewing same values from previous cycles)

snaive <- snaive(training_partition, h = length(validation_partition))



# Below is Trailing Moving Average forecast with window size of 5

Trailing_moving_average <- ma(training_partition, order = 5)



#-----------------------------------------------------------------------------------------------

# Question: g.	Put training, validation, and validation forecasts with naïve, snaive, and trailing moving average in one plot.

# Answer: 

# Below i am Plotting the training data
plot(training_partition, main = "Training, Validation, and Forecasts (Naïve, sNaïve, Trailing Moving Avg)", 
     ylab = "Births", xlab = "Time", col = "blue", xlim = c(1948, 1980))

# Below i am Adding the validation data
lines(validation_partition, col = "red", lwd = 2)

# Below i am Adding the Naïve forecast
lines(naive$mean, col = "orange", lwd = 2, lty = 2)

# Below i am Adding the sNaïve forecast
lines(snaive$mean, col = "green", lwd = 2, lty = 3)

# Below i am Adding the Trailing Moving Average forecast
lines(Trailing_moving_average, col = "purple", lwd = 2, lty = 4)

# Below i am Adding the legend to distinguish between plots.

legend("topright", legend = c("Training", "Validation", "Naïve Forecast", "sNaïve Forecast", " Trailing Moving Avg"),
       col = c("blue", "red", "purple", "green", "orange"), lty = c(1, 1, 2, 3, 4), lwd = 2)



#----------------------------------------------------------------------------------------------------------

# Question: h.	Get the RMSE, MAE, and MAPE of all models. As a comment in R answer which model gives a better performance.


# Answer: 

# Below i am calculating the accuracy metrics for each model to get RMSE, MAE, and MAPE. 

#For Naive
naive_metrics <- accuracy(naive, validation_partition)
print(naive_metrics)

#For snaive
snaive_metrics <- accuracy(snaive, validation_partition)
print(snaive_metrics)


# Below i am calculating metrics of Trailing Moving Average manually, as it is not directly a forecast object


# I am calculating the moving average directly with starting point
Trailing_moving_average_forecast_values <- ts(na.omit(Trailing_moving_average), 
                                              start = start(validation_partition), 
                                              frequency = frequency(validation_partition))

# I am Ensuring that length matches the validation partition because it creates errors if not. 

Trailing_moving_average_forecast_values <- window(Trailing_moving_average_forecast_values, 
                                                  end = end(validation_partition))

#  I am calculating the errors

Trailing_moving_average_errors <- validation_partition - Trailing_moving_average_forecast_values

#  I am calculating RMSE, MAE, and MAPE for Trailing moving average

RMSE_Trailing_moving_average <- sqrt(mean(Trailing_moving_average_errors^2, na.rm = TRUE))
MAE_Trailing_moving_average <- mean(abs(Trailing_moving_average_errors), na.rm = TRUE)
MAPE_Trailing_moving_average <- mean(abs(Trailing_moving_average_errors / validation_partition) * 100, na.rm = TRUE)

# I am Displaying the results

print(RMSE_Trailing_moving_average)
print(MAE_Trailing_moving_average)
print(MAPE_Trailing_moving_average)


# From viewing all the results, snaive model gives best performance among three as it has low RMSE, MAE, MAPE values of all. 

#--------------------------------------------------------------------------------------------------------------------------------------------------

# Question: i.	Create a forecast for 11 months in the future. Plot the forecast.

#Answer: 

# Below i am forecasting for 11 months for the future using the sNaive model as it seems to be the best

future_forecast <- snaive(births_time_series, h = 11)


# Plot the forecast
plot(future_forecast, main="sNaive Forecast for Next 11 Months", 
     ylab="Births (in thousands)", xlab="Time", col="red")


