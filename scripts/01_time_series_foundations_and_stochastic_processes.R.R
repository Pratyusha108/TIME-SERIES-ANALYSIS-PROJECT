#Time Series Analytics - Assignment-1 (PART-1)

#------------------------------------------------------------------------------------------------------------------------------------------------------

#Question-1: s a comment in your R script, provide the answers to the following questions.  Assume a time series S consists of t observations represented as (Y┤|  y_i∈S).   means “a member of” and y_i represents an individual observation. Assume S has t observations.
#Identify and write the notation for: 

# Answer: Assuming S = {y_1, y_2, .....y_t}

# Any one of the observation : y_2
# First observation: y_1
# Middle Observation: y_t/2
# Last observation: y_t
# First forcasted observation = F_{t+1}
# kth forecasted observation = F_{t+k}
# Error of kth forcasted observation: e_{t+k} = y_{t+k} - F_{t+k}
# Assuming forcast horizon is h, list of forcasted observations is as h = F_{t+1}, F_{t+2}....F_{t+h}. 

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Question-2: In the same R script you created above, generate 1000 random numbers with normal distribution and complete the following

#Answer: 
# a.	Transform this dataset to time series using the ts() function. 

set.seed(123)
white_noise <- rnorm(1000, mean = 0, sd = 1)
white_noise_ts <- ts(white_noise)

# b.	Plot the time series using plot() function

plot(white_noise_ts, main = "Gaussian White Noise")

# c.	What is the start, end, and the frequency of this time series?

start(white_noise_ts)   
end(white_noise_ts)     
frequency(white_noise_ts) 

# d.	Show that the series is “ Gaussian White Noise”. Explain why it is “Gaussian” in a short comment in your code.

# It is Gaussian White noise because we used rnorm() with data having mean=0, standard deviation=1. And plot is bell shaped. 

# e.	Save your dataset as “white_noise.csv”

write.csv(white_noise, "white_noise.csv", row.names = FALSE)


#----------------------------------------------------------------------------------------------------------------------------------------------------


#Question-3: Using the same R script as above, open the “white_noise.csv” file you created previously and load it into R. 

#Answer: 

# A) 
# Using R, generate 500 observations where each observation represents the following autoregressive equation:
#y_t= 0.4y_(t-1)-0.8y_(t-2)+w_t   
#where t represents any time index between 1:500 and
#y_t  is the observation value at time t
#w_t is the white noise value at the time t

wn <- read.csv("white_noise.csv")$x
y <- numeric(500)

y[1:2] <- w[1:2]  # I am setting first two values of y to the first two values from white noise 

for (t in 3:500) {
  y[t] <- 0.4 * y[t - 1] - 0.8 * y[t - 2] + wn[t]
}      # generating 500 observations where each observation represents the following autoregressive equation


# B) Save your dataset as “autoregressive_2.csv”
write.csv(y, "autoregressive_2.csv", row.names = FALSE) # Saving dataset as autoregressivie_2.csv

#-----------------------------------------------------------------------------------------------------------------------------------------------

# Question-4: Using the same R script as above, open the “white_noise.csv” file you created in question 2

#Answer: 

# A) Using R, generate 500 observations with the random walk feature y_t= y_(t-1)+w_t

wn <- read.csv("white_noise.csv")$x
w <- wn[1:500]
random_walk <- cumsum(w)


# B) Using R, generate 500 observations with the random walk with drift of 0.3? y_t= 〖0.3+ y〗_(t-1)+w_t

drift <- 0.3
random_walk_drift <- cumsum(w + drift)


# C) Show both random walk and random walk with drift of part a and b in one plot.

random_walk_ts <- ts(random_walk)
random_walk_drift_ts <- ts(random_walk_drift)

ts.plot(random_walk_ts, random_walk_drift_ts,
        col = c("blue", "green"),
        main = "Random Walk vs Random Walk with Drift (0.3)",
        ylab = "Value", xlab = "Time")
legend("topleft", legend = c("Random Walk", "With Drift = 0.3"),
       col = c("blue", "green"), lty = 1)


#--------------------------------------------------------------------------------------------------------------------------------------------------

# Question-5: If a timeseries dataset follows a general sinusoidal waveform as "A cos⁡(2πωt+φ)" then

#Answer: 


# A) What are the amplitude, periodical frequency, and the phase of the following equation x_t=5 cos⁡(π (t+25)/25)+ w_t

# Amplitude = 5 i.e will oscillate between -5 and +5
# Periodical frequency = 1/50 Herz. 
# phase = pi

# Note: Above is found by simplifying the equation and comparing it to standard sinusoidal form.



# B) Using R, generate a plot of 500 observations of this equation (t, 1:500), with and without white noise.
# 2 cos⁡(π(t/25+25/25)) 
# 2cos(2πt 1/50+50π/50)
# φ= π
# Therefore: A = 5, ω=1/50, and φ= π

t <- 1:500

# Below is the White noise from the previously saved file
wn <- read.csv("white_noise.csv")$x[1:500]

# Equation: y(t) = 2 * cos(2π * t * (1/50) + pi)

signal <- 2 * cos(2 * pi * t * (1/50) + pi)

# With white noise added
signal_noisy <- signal + wn

# Plot both
par(mfrow = c(2, 1))  # Two vertical plots

plot.ts(signal,
        main = "Pure Signal: y(t) = 2 * cos(2* pi * t * 1/50 + pi)",
        ylab = "Amplitude", xlab = "Time")

plot.ts(signal_noisy,
        main = "Signal with White Noise",
        ylab = "Amplitude", xlab = "Time")

#------------------------------------------------------------------------------------------------------------------------------------------------------



ASSIGNMENT - (PART-2)

---------------------------

# QUESTION- 1.	Impact of September 11 on Air Travel in the United States: The Re search and Innovative Technology Administration’s Bureau of Transportation Statistics (BTS) conducted a study to evaluate the impact of the September 11, 2001, terrorist attack on U.S. transportation. The study report and the data can be found at www.bts.gov/publications/estimated_impacts_of_9_11_on_ us_travel. 

# The goal of the study was stated as follows: The purpose of this study is to provide a greater understand ing of the passenger travel behavior patterns of persons making long distance trips before and after September 11. 

# The report analyzes monthly passenger movement data be tween January 1990 and April 2004. Data on three monthly time series are given in the file Sept11Travel.xls for this pe riod: (1) actual airline revenue passenger miles (Air), (2) rail passenger miles (Rail), and (3) vehicle miles traveled (Auto).

# In order to assess the impact of September 11, BTS took the following approach: Using data before September 11, it fore casted future data (under the assumption of no terrorist at tack). Then, BTS compared the forecasted series with the actual data to assess the impact of the event. 

# Plot each of the three pre-event time series (Air, Rail, Car). 


# Loading library
  library(readxl)

# Loading the data
travel_data <- read_excel("C:/Users/saipr/OneDrive/Desktop/Time Series Analytics/Sept11Travel.xlsx")

# Converting to time series
Air <- ts(travel_data$`Air RPM (000s)`, start = c(1990, 1), frequency = 12)
Rail <- ts(travel_data$`Rail PM`, start = c(1990, 1), frequency = 12)
Car <- ts(travel_data$`VMT (billions)`, start = c(1990, 1), frequency = 12)

# Plotting each of the three pre-event time series (Air, Rail, Car). 
par(mfrow = c(3,1))
plot(Air, main = "Air Passenger Miles", ylab = "Miles", xlab = "Time")
plot(Rail, main = "Rail Passenger Miles", ylab = "Miles", xlab = "Time")
plot(Car, main = "Auto Miles", ylab = "Miles", xlab = "Time")

  
# (a) What time series components appear from the plot? 

# y_t = Level + Trend + Seasonality + Noise (Additive)

# We can see level, trend, seasonality from the plots.  

  
# (b) What type of trend appears? Change the scale of the series, add trend lines, and suppress seasonality to better visualize the trend pattern.


# Load forecast library
library(forecast)

# Apply quadratic trend to air travel
Air_trend <- tslm(Air ~ trend + I(trend^2))

# Plot with fitted line
plot(Air, main = "Air Miles with Trend")
lines(Air_trend$fitted, col = "red", lwd = 2)

# Suppress seasonality by aggregating yearly
Air_yr <- aggregate(Air, nfrequency = 1, FUN = mean)
plot(Air_yr, main = "Yearly Air Travel (Suppressed Seasonality)")

#Note: From the observing both plots, we can say that trend for Air travel has increased significantly from 1990 to 2000 before stabilizing a bit from 2000. 

#Same goes for Rail

library(forecast)

# Apply quadratic trend to air travel
Rail_trend <- tslm(Rail ~ trend + I(trend^2))

# Plot with fitted line
plot(Rail, main = "Rail Miles with Trend")
lines(Rail_trend$fitted, col = "red", lwd = 2)

# Suppress seasonality by aggregating yearly
Rail_yr <- aggregate(Rail, nfrequency = 1, FUN = mean)
plot(Rail_yr, main = "Yearly Rail Travel (Suppressed Seasonality)")

#Note: For rails, from 1990 to 1996, there is a significant decrease in the trend and very low, then a slight variating increase from 2000. 


# Same goes for car 

library(forecast)

# Apply quadratic trend to air travel
Car_trend <- tslm(Car ~ trend + I(trend^2))

# Plot with fitted line
plot(Car, main = "Car Miles with Trend")
lines(Car_trend$fitted, col = "red", lwd = 2)

# Suppress seasonality by aggregating yearly
Car_yr <- aggregate(Car, nfrequency = 1, FUN = mean)
plot(Car_yr, main = "Yearly Car Travel (Suppressed Seasonality)")

#Note: Trend has been constantly increasing for cars from 1990 to 2004. 


# Conclusion: 
# From the observing both plots, we can say that trend for Air travel has increased significantly,
# from 1990 to 2000 before stabilizing a bit from 2000.
# For rails, from 1990 to 1996, there is a significant decrease in the trend and very low,
# then a slight variating increase from 2000. 
# Trend has been constantly increasing for cars from 1990 to 2004

  
#---------------------------------------------------------------------------------------------------------------------------------------------------

# Question-5: 5. Souvenir Sales: The file SouvenirSales.xls contains monthly sales for a souvenir shop at a beach resort town in Queensland, Australia, between 1995 and 2001.8 Back in 2001, the store wanted to use the data to forecast sales for the next 12 months (year 2002). They hired an analyst to generate forecasts. The analyst first partitioned the data into training and validation periods, with the validation period containing the last 12 months of data (year 2001). She then fit a regression model to sales, using the training period. 

#Answer: 
# (a) Create a well-formatted time plot of the data.  
#Loading the file using the path
Souvenir <- read.csv("C:/Users/saipr/Downloads/SouvenirSales.csv")

# Converting to the time series
Sales_ts <- ts(souvenir$Sales, start = c(1995, 1), frequency = 12)

# Plotting the time series
plot(Sales_ts, main = "Souvenir Sales Time Series from 1995 to 2001", ylab = "Sales", xlab = "Time")

-------------------------------------------------------------------------------------------------------------
# (b) Change the scale of the x axis, y axis, or both to logarith mic scale in order to achieve a linear relationship. Select the time plot that seems most linear. 


sales <- Souvenir$Sales

t <- 1:length(sales)


# Applying the changed scales to the axis. 
log_y <- log(sales)
log_x <- log(t)

# Set up plotting area
par(mfrow = c(2, 2))  # 4 plots

# Plot 1: Original
plot(t, sales, type = "l", main = "Original: y vs t", ylab = "Sales", xlab = "Time")

# Plot 2: Log(Y) vs t
plot(t, log_y, type = "l", main = "Log(Y) vs t", ylab = "log(Sales)", xlab = "Time")

# Plot 3: Y vs log(t)
plot(log_x, sales, type = "l", main = "Y vs log(t)", ylab = "Sales", xlab = "log(Time)")

# Plot 4: Log–Log
plot(log_x, log_y, type = "l", main = "Log–Log: log(Y) vs log(t)", ylab = "log(Sales)", xlab = "log(Time)")


#Conclusion: 

# Among the above plots, the plot (b), which is Log(Y) vs t, seems to be more linear so i select that plot.

--------------------------------------------------------------------------------------------------------------------


# (c) Comparing the two time plots, what can be said about the type of trend in the data?


# Comparing the two plots as below 

#Original Plot 
plot(Sales_ts, main = "Souvenir Sales Time Series from 1995 to 2001", ylab = "Sales", xlab = "Time")

#Selected Plot 
plot(t, log_y, type = "l", main = "Log(Y) vs t", ylab = "log(Sales)", xlab = "Time")

#CONCLUSION: 

# From looking at the above plots together the type of trend in the data is exponential, 
# Because Orginal plot is exponential and also the plot we transformed is linear upwards. 



-----------------------------------------------------------------------------------------------------
# THANK YOU !   
  
  
# SAI PRATYUSHA GORAPLLI 

#----------------------------------------------------------------------------------------------




                                    #THE END#