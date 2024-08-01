######################
# W.L.
# 07/30/2024
# Uber Data Analysis
######################

# Working directory
setwd("C:/Users/name/OneDrive/Desktop/Uber Data Analysis")

# Importing packages

library(ggplot2) # Visualization plots
library(ggthemes) # Creation of themes/scales
library(lubridate) # Separate time categories
library(dplyr)
library(tidyr) # Tidy the data
library(DT) # Utilizing datatables
library(scales) # Mapping data to correct scales

# Colors for plots

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840",
           "#0683c9", "#e075b0")

# Read the data

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data, may_data, jun_data,
                   jul_data, aug_data, sep_data) # Binding all datatables

# Visualization

head(data_2014)

# Data Structure

str(data_2014)

# Conducting Analysis

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

# Summary Statistics

summary(data_2014)

# Extraction of time from datetime

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"),
                         format = "%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time) # Formatting
data_2014$day <- format(day(data_2014$Date.Time)) # Day
data_2014$month <- format(month(data_2014$Date.Time, label = TRUE)) # Month
data_2014$year <- format(year(data_2014$Date.Time)) 
data_2014$dayofweek <- format(wday(data_2014$Date.Time, label = TRUE)) # Day of week

# H/M/S

data_2014$hour <- factor(hour(hms(data_2014$Time))) # Converted as factors
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

# Complete data

head(data_2014)

# Visualizations

hour_data <- data_2014 %>% 
  group_by(hour) %>%
  summarise(Total = n())  # Grouping data hour and count

# Tabular form

datatable(hour_data)

# Visualizing the data

ggplot(hour_data, aes(hour, Total)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "gold") +
  ggtitle("Trips by Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

###############################

month_hour_data <- data_2014 %>% 
  group_by(month, hour) %>%
  summarise(Total = n()) 

# Tabular form

datatable(month_hour_data)

# Plot

ggplot(month_hour_data, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma) 

# Analysis for September rides

sept_hour <- data_2014 %>%
  group_by(hour, month) %>%
  filter(month == "Sep") %>%
  summarise(Total = n())

ggplot(sept_hour, aes(hour, Total, fill = hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month for Sept.") +
  scale_y_continuous(labels = comma) 

# Analysis for April rides

april_hour <- data_2014 %>%
  group_by(hour, month) %>%
  filter(month == "Apr") %>%
  summarise(Total = n())

ggplot(april_hour, aes(hour, Total, fill = hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month for Apr.") +
  scale_y_continuous(labels = comma) 

# Plotting the data grouped by day

day_data <- data_2014 %>% 
  group_by(day) %>%
  summarise(Total = n())  # Grouping data hour and count

# Tabular form

datatable(day_data)

# Visualization

ggplot(day_data, aes(day, Total)) +
  geom_bar(stat = "identity", fill = "lightgrey", color = "black") +
  ggtitle("Trips by Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# Month/Day Grouping

month_day_data <- data_2014 %>% 
  group_by(month, day) %>%
  summarise(Total = n()) 

# Tabular form

datatable(month_day_data)

# Plot

ggplot(month_day_data, aes(day, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma)





