---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
library(ggplot2)
library(scales)
if (!file.exists("figure")) dir.create("figure")
if (!file.exists("activity.csv")) unzip("activity.zip")
activity_data <- read.csv("activity.csv")
activity_data$date <- as.Date(activity_data$date,"%Y-%m-%d")
activity_data$time <- as.POSIXct(formatC(activity_data$interval, 
                                         width=4, flag="0"), format="%H%M")
```

## What is mean total number of steps taken per day?

1) Calculate the total number of steps taken per day

```r
activity_per_day <- activity_data %>% group_by(date) %>% 
    summarize(total.steps = sum(steps,na.rm=TRUE))
print(activity_per_day, n = nrow(activity_per_day))
```

```
## # A tibble: 61 x 2
##    date       total.steps
##    <date>           <int>
##  1 2012-10-01           0
##  2 2012-10-02         126
##  3 2012-10-03       11352
##  4 2012-10-04       12116
##  5 2012-10-05       13294
##  6 2012-10-06       15420
##  7 2012-10-07       11015
##  8 2012-10-08           0
##  9 2012-10-09       12811
## 10 2012-10-10        9900
## 11 2012-10-11       10304
## 12 2012-10-12       17382
## 13 2012-10-13       12426
## 14 2012-10-14       15098
## 15 2012-10-15       10139
## 16 2012-10-16       15084
## 17 2012-10-17       13452
## 18 2012-10-18       10056
## 19 2012-10-19       11829
## 20 2012-10-20       10395
## 21 2012-10-21        8821
## 22 2012-10-22       13460
## 23 2012-10-23        8918
## 24 2012-10-24        8355
## 25 2012-10-25        2492
## 26 2012-10-26        6778
## 27 2012-10-27       10119
## 28 2012-10-28       11458
## 29 2012-10-29        5018
## 30 2012-10-30        9819
## 31 2012-10-31       15414
## 32 2012-11-01           0
## 33 2012-11-02       10600
## 34 2012-11-03       10571
## 35 2012-11-04           0
## 36 2012-11-05       10439
## 37 2012-11-06        8334
## 38 2012-11-07       12883
## 39 2012-11-08        3219
## 40 2012-11-09           0
## 41 2012-11-10           0
## 42 2012-11-11       12608
## 43 2012-11-12       10765
## 44 2012-11-13        7336
## 45 2012-11-14           0
## 46 2012-11-15          41
## 47 2012-11-16        5441
## 48 2012-11-17       14339
## 49 2012-11-18       15110
## 50 2012-11-19        8841
## 51 2012-11-20        4472
## 52 2012-11-21       12787
## 53 2012-11-22       20427
## 54 2012-11-23       21194
## 55 2012-11-24       14478
## 56 2012-11-25       11834
## 57 2012-11-26       11162
## 58 2012-11-27       13646
## 59 2012-11-28       10183
## 60 2012-11-29        7047
## 61 2012-11-30           0
```

2) If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
# save as png
png("figure/plot1.png")
ggplot(data = activity_per_day, aes(x = date, y = total.steps)) +
      geom_histogram(stat = "identity", fill = "purple") +
      scale_x_date(labels=date_format("%d-%b"), breaks = "1 week") +
      labs(title = "Total number of steps taken per day",
           x = "Date", y = "Total number of steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# display for report
ggplot(data = activity_per_day, aes(x = date, y = total.steps)) +
      geom_histogram(stat = "identity", fill = "purple") +
      scale_x_date(labels=date_format("%d-%b"), breaks = "1 week") +
      labs(title = "Total number of steps taken per day",
           x = "Date", y = "Total number of steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3) Calculate and report the mean and median of the total number of steps taken per day

```r
paste("mean:", mean(activity_per_day$total.steps))
```

```
## [1] "mean: 9354.22950819672"
```

```r
paste("median:", median(activity_per_day$total.steps))
```

```
## [1] "median: 10395"
```

## What is the average daily activity pattern?

1) Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
activity_by_time_of_day <- activity_data %>% group_by(time) %>% summarize(steps = mean(steps, na.rm = TRUE))
# save as png
png("figure/plot2.png")
plot(steps ~ time, data = activity_by_time_of_day, type = "l")
dev.off()
```

```
## png 
##   2
```

```r
# display for report
plot(steps ~ time, data = activity_by_time_of_day, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
index_max_steps <- which.max(activity_by_time_of_day$steps)
paste("Time interval of max average steps: ",
      format(activity_by_time_of_day$time[index_max_steps],"%H:%M"), 
      "(index: ", index_max_steps, ")")
```

```
## [1] "Time interval of max average steps:  08:35 (index:  104 )"
```

## Imputing missing values
1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs

```r
paste("Number of missing values: ", sum(is.na(activity_data)))
```

```
## [1] "Number of missing values:  2304"
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# Find mean steps for each interval and create a new column with these means
activity_data2 <- within(activity_data, 
                        intervalMean <- ave(steps, interval, FUN = function(x) mean(x, na.rm = TRUE)))
```

3) Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# replace NAs with intervalMean
activity_data2$steps[is.na(activity_data2$steps)] <- activity_data2$intervalMean[is.na(activity_data2$steps)]
head(activity_data2)
```

```
##       steps       date interval                time intervalMean
## 1 1.7169811 2012-10-01        0 2020-02-19 00:00:00    1.7169811
## 2 0.3396226 2012-10-01        5 2020-02-19 00:05:00    0.3396226
## 3 0.1320755 2012-10-01       10 2020-02-19 00:10:00    0.1320755
## 4 0.1509434 2012-10-01       15 2020-02-19 00:15:00    0.1509434
## 5 0.0754717 2012-10-01       20 2020-02-19 00:20:00    0.0754717
## 6 2.0943396 2012-10-01       25 2020-02-19 00:25:00    2.0943396
```

4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity_per_day2 <- activity_data2 %>% group_by(date) %>% 
    summarize(total.steps = sum(steps,na.rm=TRUE))
png("figure/plot3.png")
ggplot(data = activity_per_day2, aes(x = date, y = total.steps)) +
      geom_histogram(stat = "identity", fill = "purple") +
      scale_x_date(labels=date_format("%d-%b"), breaks = "1 week") +
      labs(title = "Total number of steps taken per day",
           x = "Date", y = "Total number of steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# display for report
ggplot(data = activity_per_day2, aes(x = date, y = total.steps)) +
      geom_histogram(stat = "identity", fill = "purple") +
      scale_x_date(labels=date_format("%d-%b"), breaks = "1 week") +
      labs(title = "Total number of steps taken per day",
           x = "Date", y = "Total number of steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
paste("mean:", mean(activity_per_day2$total.steps))
```

```
## [1] "mean: 10766.1886792453"
```

```r
paste("median:", median(activity_per_day2$total.steps))
```

```
## [1] "median: 10766.1886792453"
```

```r
print("The mean and median are now the same - removing the NA values has eliminated evidence of skewness in the distribution")
```

```
## [1] "The mean and median are now the same - removing the NA values has eliminated evidence of skewness in the distribution"
```

## Are there differences in activity patterns between weekdays and weekends?

1) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
wkdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity_data2$isWeekday <- factor((weekdays(activity_data2$date) %in% wkdays), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

2) Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
library(reshape2)
weekday_data <- activity_data2 %>% subset(isWeekday == "weekday") %>% group_by(interval) %>% summarize(weekday.mean.steps = mean(steps,na.rm=TRUE))
weekend_data <- activity_data2 %>% subset(isWeekday == "weekend") %>% group_by(interval) %>% summarize(weekend.mean.steps = mean(steps,na.rm=TRUE))
combo_df <- melt(data.frame(interval=weekday_data$interval, 
                            weekday=weekday_data$weekday.mean.steps, 
                            weekend=weekend_data$weekend.mean.steps),
                 id.vars = "interval")
colnames(combo_df) <- c("interval","isWeekday","mean.steps")
png("figure/plot4.png")
xyplot(mean.steps ~ interval | isWeekday, 
       group = isWeekday, data = combo_df,
       type = "l",
       layout = c(1, 2),
       col  = "blue",
       xlab = "Interval", ylab = "Number of steps")
dev.off()
```

```
## png 
##   2
```

```r
# display for report
xyplot(mean.steps ~ interval | isWeekday, 
       group = isWeekday, data = combo_df,
       type = "l",
       layout = c(1, 2),
       col  = "blue",
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
