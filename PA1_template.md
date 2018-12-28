---
title: "Reproducible Research: Peer Assessment 1"
author: "Susmitha Saripalli"
output: 
  html_document:
    keep_md: true
---
## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing the data


```r
# Loading libraries
library("data.table")
library(ggplot2)

# Loading data
dataUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(dataUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

# Read CSV
d <- fread(input = "data/activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
steps <- d[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 


```r
ggplot(steps, aes(x = steps)) +
    geom_histogram(fill = "seagreen3", binwidth = 1000) +
    labs(title = "Steps per Day", x = "Steps", y = "Number of Days")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps Median_Steps
## 1:   10766.19        10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalD <- d[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(intervalD, aes(x = interval , y = steps)) + 
  geom_line(color="seagreen3", size=1) + 
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalD[steps == max(steps), .(max_interval = interval)]
```

```
##    max_interval
## 1:          835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
d[is.na(steps), .N ]
```

```
## [1] 2304
```

```r
# alternative solution
nrow(d[is.na(steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Filling in missing values with median of dataset. 
d[is.na(steps), "steps"] <- d[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
fwrite(x = d, file = "data/tidyData.csv", quote = FALSE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
steps <- d[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

# mean and median total number of steps taken per day
steps[, .(meanSteps = mean(steps), medSteps = median(steps))]
```

```
##    meanSteps medSteps
## 1:   9354.23    10395
```

```r
ggplot(steps, aes(x = steps)) + geom_histogram(fill = "seagreen3", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

(with na):
mean steps: 10765
median steps: 10765
(fillin in na with median):
mean steps: 9354.23
median stepsL 10395

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# restart d, just in case 
d <- fread(input = "data/activity.csv")
d[, date := as.POSIXct(date, format = "%Y-%m-%d")]
d[, `Day`:= weekdays(x = date)]
d[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day`), "dayOfWeek"] <- "weekday"
d[grepl(pattern = "Saturday|Sunday", x = `Day`), "dayOfWeek"] <- "weekend"
d[, `dayOfWeek` := as.factor(`dayOfWeek`)]
head(d, 10)
```

```
##     steps       date interval    Day dayOfWeek
##  1:    NA 2012-10-01        0 Monday   weekday
##  2:    NA 2012-10-01        5 Monday   weekday
##  3:    NA 2012-10-01       10 Monday   weekday
##  4:    NA 2012-10-01       15 Monday   weekday
##  5:    NA 2012-10-01       20 Monday   weekday
##  6:    NA 2012-10-01       25 Monday   weekday
##  7:    NA 2012-10-01       30 Monday   weekday
##  8:    NA 2012-10-01       35 Monday   weekday
##  9:    NA 2012-10-01       40 Monday   weekday
## 10:    NA 2012-10-01       45 Monday   weekday
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
d[is.na(steps), "steps"] <- d[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
intervalD <- d[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `dayOfWeek`)] 

ggplot(intervalD , aes(x = interval , y = steps, color=`dayOfWeek`)) + 
  geom_line() + 
  labs(title = "Avg. Daily Steps by Weekday/Weekend", x = "Interval", y = "No. of Steps") + 
  facet_wrap(~`dayOfWeek` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggplot(intervalD , aes(x = interval , y = steps, color=`dayOfWeek`)) + 
  geom_line() + 
  labs(title = "Avg. Daily Steps by Weekday/Weekend", x = "Interval", y = "No. of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-2.png)<!-- -->
