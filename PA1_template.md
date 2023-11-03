---
title: "Reproducible Research: Peer Assessment "
author: "Gabriel Cirino"
date: "2023-11-03"
output: html_document
---



## Loading and preprocessing the data
# Load the necessary libraries
library(ggplot2)
library(dplyr)

# Load the data from a CSV file
activity <- read.csv("activity.csv")

# Preprocess/transform the data if necessary
# Convert the date from character to Date object
activity$date <- as.Date(activity$date, "%Y-%m-%d")


## What is mean total number of steps taken per day?
# Calculate the total number of steps taken per day
daily_steps <- activity %>% 
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))

# Create a histogram of the total number of steps taken each day
hist(daily_steps$total_steps, main = "Total Steps per Day", xlab = "Number of Steps", breaks = 20)

# Calculate and report the mean and median total number of steps taken per day
mean_steps <- mean(daily_steps$total_steps, na.rm = TRUE)
median_steps <- median(daily_steps$total_steps, na.rm = TRUE)



## What is the average daily activity pattern?
# Time series plot of the 5-minute interval and the average number of steps taken
interval_steps <- activity %>% 
  group_by(interval) %>%
  summarize(average_steps = mean(steps, na.rm = TRUE))

ggplot(interval_steps, aes(x = interval, y = average_steps)) + 
  geom_line() +
  labs(title = "Average Number of Steps per Interval", x = "Interval", y = "Average Steps")
  
# Identifying the interval with the maximum number of steps
max_interval <- interval_steps[which.max(interval_steps$average_steps),]$interval



## Imputing missing values
# Calculate and report the total number of missing values in the dataset
total_na <- sum(is.na(activity$steps))

# Impute missing values by using the mean for that 5-minute interval
activity_imputed <- activity
for (i in 1:nrow(activity_imputed)) {
  if (is.na(activity_imputed$steps[i])) {
    activity_imputed$steps[i] <- interval_steps$average_steps[which(interval_steps$interval == activity_imputed$interval[i])]
  }
}

# Recalculate the total number of steps taken per day with imputed data
daily_steps_imputed <- activity_imputed %>% 
  group_by(date) %>%
  summarize(total_steps = sum(steps))

# Histogram of the total number of steps taken each day after imputing missing values
hist(daily_steps_imputed$total_steps, main = "Total Steps per Day (Imputed Data)", xlab = "Number of Steps", breaks = 20)

# Recalculate mean and median steps per day after imputing
mean_steps_imputed <- mean(daily_steps_imputed$total_steps)
median_steps_imputed <- median(daily_steps_imputed$total_steps)




## Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable for weekday vs weekend
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Calculate average steps per interval across weekdays and weekends
interval_steps_day_type <- activity_imputed %>% 
  group_by(interval, day_type) %>%
  summarize(average_steps = mean(steps))

# Panel plot for weekdays and weekends
ggplot(interval_steps_day_type, aes(x = interval, y = average_steps, color = day_type)) + 
  geom_line() +
  facet_wrap(~day_type, ncol = 1, scales = "free_y") +
  labs(title = "Activity Patterns: Weekdays vs Weekends", x = "Interval", y = "Average Number of Steps")

