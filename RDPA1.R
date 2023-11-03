# Load the necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
activity <- read.csv("activity.csv")

# Preprocess/transform the data if necessary
# For instance, convert the date from a string to a Date object
activity$date <- as.Date(activity$date, "%Y-%m-%d")

# Calculate the total number of steps taken per day
daily_steps <- activity %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))

# Create a histogram of the total number of steps taken each day
ggplot(daily_steps, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "white") +
  labs(title = "Total Number of Steps Taken Each Day", x = "Number of Steps", y = "Frequency")

# Calculate and report the mean and median of the total number of steps taken per day
mean_steps <- mean(daily_steps$total_steps)
median_steps <- median(daily_steps$total_steps)

# Create a time series plot of the 5-minute interval and the average number of steps taken
interval_steps <- activity %>%
  group_by(interval) %>%
  summarize(average_steps = mean(steps, na.rm = TRUE))

ggplot(interval_steps, aes(x = interval, y = average_steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern", x = "5-minute Interval", y = "Average Number of Steps")

# Identify the 5-minute interval that on average contains the maximum number of steps
max_interval <- interval_steps[which.max(interval_steps$average_steps), ]

# Calculate the total number of missing values in the dataset
total_na <- sum(is.na(activity$steps))

# Impute missing values (for example, by using the mean for that 5-minute interval)
activity_imputed <- activity
for (i in 1:nrow(activity_imputed)) {
  if (is.na(activity_imputed$steps[i])) {
    activity_imputed$steps[i] <- interval_steps$average_steps[which(interval_steps$interval == activity_imputed$interval[i])]
  }
}

# Continue with the analysis using the imputed dataset...

# Commit the Rmd file, render the markdown and HTML files, commit the figures directory, push to GitHub, and submit the URL and SHA-1 hash as instructed.
