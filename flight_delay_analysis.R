# Project 1: Flight Delays Analysis
# Author : Suprit Kulkarni

# 1. INSTALL AND LOAD THE REQUIRED PACKAGES
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("janitor")
install.packages("dplyr")

library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(janitor)
library(dplyr)


# 2. LOAD THE DATASET AND INSPECT STRUCTURE OF DATA
flight_delays_df <- read_excel("FlightDelays/1657873325_flightdelays.xlsx")
str(flight_delays_df)          # View structure
head(flight_delays_df)         # Preview first few rows

glimpse(flight_delays_df)                # Overview of columns
summary(flight_delays_df)                # Descriptive summary

# 3. Find missing values in each column
sapply(flight_delays_df, function(x) sum(is.na(x)))

# 4. Scheduled time, Carrier, Destination, Origin, Weather, Day of Week
ggplot(flight_delays_df, aes(x = schedtime)) + geom_histogram(binwidth=1, fill="skyblue") + labs(title="Histogram of Scheduled Time")
ggplot(flight_delays_df, aes(x = carrier)) + geom_bar(fill="orange") + labs(title="Flights by Carrier")
ggplot(flight_delays_df, aes(x = dest)) + geom_bar(fill="green") + labs(title="Flights by Destination")
ggplot(flight_delays_df, aes(x = origin)) + geom_bar(fill="purple") + labs(title="Flights by Origin")
ggplot(flight_delays_df, aes(x = weather)) + geom_bar(fill="gray") + labs(title="Weather Conditions")
ggplot(flight_delays_df, aes(x = dayweek)) + geom_bar(fill="brown") + labs(title="Day of the Week Distribution")

# 5. Scatter plot for flights on time and delayed
ggplot(flight_delays_df, aes(x = schedtime, y = deptime, color = factor(delay))) +
  geom_point(alpha = 0.4) +
  labs(title = "Scheduled Time vs Departure Time by Delay Status", color = "Delay (0=On Time, 1=Delayed)")

# 6. Box plot to understand how many days in a month flights are delayed by what time
ggplot(flight_delays_df, aes(x = factor(daymonth), y = deptime, fill = factor(delay))) +
  geom_boxplot() +
  labs(title = "Departure Time Distribution by Day of Month and Delay Status", x = "Day of Month", y = "Departure Time", fill = "Delay")

# 7. Define hours of departure for analysis

flight_delays_df <- flight_delays_df %>%
  mutate(dep_hour = floor(deptime))
ggplot(flight_delays_df, aes(x = dep_hour)) +
  geom_histogram(binwidth=1, fill="steelblue") +
  labs(title="Departure Hour Distribution")

# 8. Categorical data using tables
# Flights count by carrier and delay status
table_carrier_delay <- table(flight_delays_df$carrier, flight_delays_df$delay)
print(table_carrier_delay)

# 9. Redefine delay variables if needed
# Recoding if delay variable isn't 0/1:
flight_delays_df <- flight_delays_df %>%
  mutate(delay_flag = if_else(delay == 0, "On Time", "Delayed"))
table(flight_delays_df$delay_flag)


# 10. Summary of major variables

summary(select(flight_delays_df, schedtime, deptime, distance))


# 11. Plot histograms of major variables

ggplot(flight_delays_df, aes(x = distance)) +
  geom_histogram(binwidth=50, fill="gold") +
  labs(title="Histogram of Flight Distance")

ggplot(flight_delays_df, aes(x = deptime)) +
  geom_histogram(binwidth=1, fill="navy") +
  labs(title="Histogram of Departure Times")



# 12. Pie chart of delayed vs on-time flights
flight_delays_df %>%
  count(delay) %>%
  ggplot(aes(x = "", y = n, fill = delay)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Flight Delay Distribution")

