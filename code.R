## code.R
## experimental R code before writing Rmarkdown document
## created : 2010-10-20

## set environment 
setwd("~/Documents/Coursera.org/Data Scientist Specializaion/Reproducible Research/Project 1/repdata-007/")

## Loading and preprocessing the data

## 1. Load the data (i.e. read.csv())
data <- read.csv('activity.csv', colClasses=c(NA, "Date", NA))

## 2. Process/transform the data (if necessary) into a format suitable 
##    for your analysis

## What is mean total number of steps taken per day?
## For this part of the assignment, you can ignore the missing values 
## in the dataset.
total <- aggregate(data$steps, list(date = data$date), FUN = sum)
total <- total[!is.na(total$x), ]   ## remove NA's

## 1. Make a histogram of the total number of steps taken each day
hist(total$x, xlab="Total number of steps", 
     main="Total number of steps taken each day")

## 2. Calculate and report the mean and median total number of steps 
##    taken per day
mean_total <- mean(total$x)
median_total <- median(total$x)

## What is the average daily activity pattern?

## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval
##    (x-axis) and the average number of steps taken, averaged across 
##    all days (y-axis)
data2 <- data[!is.na(data$steps), ]   # remove NA's
avg <- aggregate(data2$steps, list(interval=data2$interval), FUN=mean)

with(avg, plot(interval, x, type="l", 
               ylab="average number of steps taken, averaged across all days"))

## 2. Which 5-minute interval, on average across all the days in the 
##    dataset, contains the maximum number of steps?
max_interval <- avg[avg$x %in% max(avg$x), ]

## Imputing missing values
## Note that there are a number of days/intervals where there are 
## missing values (coded as NA). The presence of missing days may 
## introduce bias into some calculations or summaries of the data.

## 1. Calculate and report the total number of missing values in the 
##    dataset (i.e. the total number of rows with NAs)
sum(is.na(data$steps))

## 2. Devise a strategy for filling in all of the missing values 
##    in the dataset. The strategy does not need to be sophisticated. 
##    For example, you could use the mean/median for that day, or 
##    the mean for that 5-minute interval, etc.

## 3. Create a new dataset that is equal to the original dataset but 
##    with the missing data filled in.
data3 <- data
missing <- which(is.na(data3$steps))   ## indices of NA steps values
mean_by_interval <- rep(avg$x, nrow(data3)/nrow(avg))
data3$steps[missing] <- mean_by_interval[missing]   ## replace NA's

## 4. Make a histogram of the total number of steps taken each day 
##    and Calculate and report the mean and median total number of 
##    steps taken per day. Do these values differ from the estimates 
##    from the first part of the assignment? What is the impact of 
##    imputing missing data on the estimates of the total daily number 
##    of steps?
total2 <- aggregate(data3$steps, list(date = data3$date), FUN = sum)
hist(total2$x, xlab="Total number of steps (NA's replaced)", 
     main="Total number of steps taken each day")

mean_total2 <- mean(total2$x)
median_total2 <- median(total2$x)

## Are there differences in activity patterns between weekdays and 
## weekends?
## For this part the weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.

## 1. Create a new factor variable in the dataset with two levels – 
##    “weekday” and “weekend” indicating whether a given date is 
##    a weekday or weekend day.

data4 <- data3
data4$date <- weekdays(as.Date(data4$date, format = "%Y-%m-%d"))
data4$weekend <- ifelse(data4$date == c("토요일", "일요일"), 
                        c("weekdend"), c("weekday"))
data4$weekend <- factor(data4$weekend)

## 2. Make a panel plot containing a time series plot (i.e. type = "l") 
##    of the 5-minute interval (x-axis) and the average number of steps 
##    taken, averaged across all weekday days or weekend days (y-axis). 
##    The plot should look something like the following, which was 
##    creating using simulated data:

avg2 <- aggregate(steps ~ interval + weekend, data = data4, mean)

library(lattice)
xyplot(steps ~ interval | weekend, avg2, type="l", layout = c(1, 2))

## Your plot will look different from the one above because you will be 
## using the activity monitor data. Note that the above plot was made 
## using the lattice system but you can make the same version of the 
## plot using any plotting system you choose.

