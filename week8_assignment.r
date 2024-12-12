install.packages("tidyverse")
library(tidyverse)
library(broom)

clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";") # nolint

View(clim)

# Inspecting table characteristics, altitude and p_mean are char type instead of integer #nolint
str(clim)

# Remove comma character and convert to integer
clim <- clim %>% mutate(
    p_mean = gsub(",", "", p_mean),
    p_mean = as.integer(p_mean),
    altitude = gsub(",", "", altitude),
    altitude = as.integer(altitude)
)
# EXERCISE 1

# remove extreme points
climfrar <- clim[1:34, ]

# model
mean_annual_temperature <- lm(t_mean ~ altitude + lat + lon, climfrar)

# The results says the relationship between temperature and longitude is not significant (p = 0.424) #nolint
# (Intercept): Considering altitude, lat, and lon to be 0, temperature is 37.3 degrees #nolint. The probability of observing 37.3 if it was actually 0 is small (7.29e-15) enough (> 0.05)
# altitude: Holding lat and lon constant, there is only a negligible chance (p = 3.17e8) of observing a -0.00641 decrease in temperature per unit increase in altitude if there true change is 0 #nolint
# lat: Holding altitude and lon constant, there is only a negligible chance (p = 1.24e-10) of observing a -0.534 decrease in temperature per unit increase in lat if the true change is 0 #nolint
# lon: Holding altitude and lat constant, there is a high chance (p = 0.424) of observing a 0.0321 increase in temperature per unit increase in lon if the true change is 0 #nolint
tidy(mean_annual_temperature)
#  term        estimate std.error statistic  p.value
#   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept) 37.3      2.62        14.2   7.29e-15
# 2 altitude    -0.00641  0.000869    -7.38  3.17e- 8
# 3 lat         -0.534    0.0558      -9.58  1.24e-10
# 4 lon          0.0321   0.0396       0.811 4.24e- 1

# EXERCISE 2
# After removing longitude, we observe a slight increase in the previous estimates for intercept, altitude, and latitude #nolint

# (Intercept): Considering altitude, to be 0, temperature is 37.9 degrees #nolint. The probability of observing 37.9 if it was actually 0 is small (5.68e-16) enough (> 0.05)
# altitude: Holding lat constant, there is only a negligible chance (p = 2.34e- 8) of observing a -0.00626 decrease in temperature per unit increase in altitude if there true change is 0 #nolint
# lat: Holding altitude constant, there is only a negligible chance (p = 1.72e-11) of observing a -0.547 decrease in temperature per unit increase in lat if the true change is 0 #nolint
mean_annual_temperature <- lm(t_mean ~ altitude + lat, climfrar)
tidy(mean_annual_temperature)

#  term        estimate std.error statistic  p.value
#   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept) 37.9      2.48         15.3  5.68e-16
# 2 altitude    -0.00626  0.000844     -7.42 2.34e- 8
# 3 lat         -0.547    0.0533      -10.3  1.72e-11

# Predict the temperatures for Mont-Ventoux and Pic-du-midi
data <- mean_annual_temperature
new_data <- data.frame(
    altitude = c(1909, 2877), # Altitudes in meters
    lat = c(44.1736, 42.9371) # Latitudes in degrees
)

# Make the prediction
predicted_temp <- predict(mean_annual_temperature, newdata = new_data)

# Display results
# Predicted temperature for Mont-Ventoux and Pic-du-midi is 1.8139 degrees and -3.574 degrees respectively. #nolint
predicted_temp
# > predicted_temp
#         1         2
#  1.813937 -3.574100


# Exercise 3

# Install and load necessary library
install.packages("scatterplot3d") # If not already installed
library(scatterplot3d)

png("3dcube.png")
# 3D scatterplot
scatterplot3d(
    x = climfrar$altitude, # x-axis: Altitude
    y = climfrar$lat, # y-axis: Latitude
    z = climfrar$t_mean, # z-axis: Mean Annual Temperature
    color = "blue", # Point color
    pch = 19, # Point shape
    main = "3D Scatterplot of Temperature vs Altitude and Latitude",
    xlab = "Altitude (m)",
    ylab = "Latitude (°)",
    zlab = "Mean Annual Temperature (°C)"
)
dev.off()

# Extract model coefficients
plane <- coef(mean_annual_temperature)
s3d$plane3d(plane) # nolint

# Model summary
summary(mean_annual_temperature)
# from the 3d plot and summary, there is a negative relationship between Mean Annual Temperature, Altitude, and Lattitude. #nolint
