---
title: 'Peer-graded Assignment: Course Project 1'
author: "Yogesh Dhar"
date: "09/04/2018"
output: 
  html_document:
    keep_md: true

---


```r
# Reproducible Research: Peer-graded Assignment: Course Project 1
# Loading the library required to produce plots
library(lattice)

# Loading and preprocessing the data
#   1. Load the data (i.e. read.csv())
#   2. Process/transform the data (if necessary) into a format suitable for your analysis
#   3. Assuming we have the file activity.csv saved in working directory

setwd("/Users/yogesh")
dir()
```

```
##  [1] "Applications"                   "BrawlhallaReplays"             
##  [3] "Desktop"                        "Documents"                     
##  [5] "Downloads"                      "Google Drive"                  
##  [7] "Library"                        "MacKeeper Backups"             
##  [9] "Movies"                         "Music"                         
## [11] "Pictures"                       "Public"                        
## [13] "Source_Classification_Code.rds" "activity.csv"                  
## [15] "addPatt.R"                      "summarySCC_PM25.rds"
```

```r
activity <- read.csv("activity.csv", header = TRUE)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset. 
#   1. Calculate the total number of steps taken per day
aggdata <- aggregate(steps~date, data= activity, sum, na.rm = TRUE)
head(aggdata)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
#   2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
hist(aggdata$steps, breaks = "Sturges", col = "green", main = "Histogram of Steps per day", xlab = "Steps per day", ylab = "No of Days", labels = TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#   3. Calculate and report the mean and median of the total number of steps taken per day (We have excluded NA values for calculation of Mean and Median)
mean(aggdata$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(aggdata$steps, na.rm = TRUE)
```

```
## [1] 10765
```

```r
# What is the average daily activity pattern?
#   1. Make a time series plot (i.e. ???????????????? = ??????????) of the 5-minute interval (x-axis)   and the average number of steps taken, averaged across all days (y-axis)
#   The average number of steps taken:
steps_interval <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
# Time series plot of the average number of steps taken through base plotting:
plot(steps~interval, data=steps_interval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
#   2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps_interval <- steps_interval[which.max(steps_interval$steps),]$interval
max_steps_interval
```

```
## [1] 835
```

```r
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as ????????). The presence of missing days may introduce bias into some calculations or summaries of the data.
#   1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)
NA_fields <- is.na(activity[,1])
# Let's check if NA fields are being computed properly (result should be 2304)
summary(NA_fields)
```

```
##    Mode   FALSE    TRUE 
## logical   15264    2304
```

```r
#   2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# I am using mean per interval here to fill NA values
mean_fill <- mean(steps_interval$steps)
mean_fill
```

```
## [1] 37.3826
```

```r
#   3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Imputing NA values with 37.3826 (mean per interval)
activity_new <- activity
activity_new[NA_fields,1]<-mean_fill
#   4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
aggdata1 <- aggregate(steps~date, data= activity_new, sum, na.rm = TRUE)
hist(aggdata1$steps, breaks = "Sturges", col = "maroon", main = "Histogram of Steps per day after missing values are imputed", xlab = "Steps per day", ylab = "Frequency", labels = TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
# Let???s find mean and median after imputing values
mean(aggdata1$steps)
```

```
## [1] 10766.19
```

```r
median(aggdata1$steps)
```

```
## [1] 10766.19
```

```r
activity_new$date <- as.Date(activity_new$date)

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part

#   1. Create a new factor variable in the dataset with two levels ??? "weekday" and "weekend"" indicating whether a given date is a weekday or weekend day
# Let us define Weekends and Weekdays based on date field
activity$week <- ifelse(weekdays(activity_new$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
# Add new column with Date_type
activity_new$Date_type <- activity$week
head(activity_new)
```

```
##     steps       date interval Date_type
## 1 37.3826 2012-10-01        0   weekday
## 2 37.3826 2012-10-01        5   weekday
## 3 37.3826 2012-10-01       10   weekday
## 4 37.3826 2012-10-01       15   weekday
## 5 37.3826 2012-10-01       20   weekday
## 6 37.3826 2012-10-01       25   weekday
```

```r
# Test if values got rightly allocated between weekends and weekdays
aggregate(steps~Date_type, data=activity_new, sum)
```

```
##   Date_type    steps
## 1   weekday 461513.1
## 2   weekend 195224.4
```

```r
#   2. Make a panel plot containing a time series plot (i.e. type = ???l???) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data
# I have tried formula with SUM and Mean both, however, I have prepared graph only with Mean
# Sum
aggdata_Day_type_interval <- aggregate(steps~interval + Date_type, data=activity_new, sum)
head(aggdata_Day_type_interval)
```

```
##   interval Date_type    steps
## 1        0   weekday 315.2956
## 2        5   weekday 242.2956
## 3       10   weekday 231.2956
## 4       15   weekday 232.2956
## 5       20   weekday 228.2956
## 6       25   weekday 283.2956
```

```r
# Formula with MEAN (We are using MEAN function here to aggregate data and create plot)
aggdata_Day_type_interval <- aggregate(steps~interval + Date_type, data=activity_new, mean)
head(aggdata_Day_type_interval)
```

```
##   interval Date_type    steps
## 1        0   weekday 7.006569
## 2        5   weekday 5.384347
## 3       10   weekday 5.139902
## 4       15   weekday 5.162124
## 5       20   weekday 5.073235
## 6       25   weekday 6.295458
```

```r
names(aggdata_Day_type_interval) <- c("interval", "Day_type", "steps")
xyplot(steps ~ interval | Day_type, aggdata_Day_type_interval, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)<!-- -->
