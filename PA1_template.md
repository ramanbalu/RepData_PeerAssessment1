
Reproducible Research: Peer Assessment 1
========================================
## Loading and preprocessing the data

```r
setwd("C:/project/Reproducible Research")
activity_data <- read.csv("activity.csv", header = T)
library(ggplot2)
```
## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
1. Make a histogram of the total number of steps taken each day

```r
result <- tapply(activity_data$steps, activity_data$date, sum, na.rm = T)
barplot(result, main = "Total number of steps", xlab = "Days", ylab = "Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" title="" alt="" width="672" />

```r
        dev.copy(png, file="C:/Project/Reproducible Research/figures/Number of steps.png", width=480, height=480)
```

```
## png 
##   4
```

```r
        dev.off()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-2.png" title="" alt="" width="672" />

```
## pdf 
##   2
```
2. Calculate and report the mean and median total number of steps taken per day

```r
mean(result,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(result,na.rm=TRUE)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
result <- tapply(activity_data$steps, activity_data$interval, mean, na.rm = T)
plot(result, type = "l", main = "Average number of steps in a day",
     xlab = "5-minute interval in a day", ylab = "Average number of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" title="" alt="" width="672" />
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
idx <- which(result == max(result))
max.interval <- names(idx)
max.interval
```

```
## [1] "835"
```
## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or 
summaries of the data.
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing.count <- sum(!complete.cases(activity_data))
missing.count
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new.activity_data <- activity_data
interval.mean <- tapply(new.activity_data$steps, new.activity_data$interval, 
                           mean, na.rm = T)
fill.steps <- function (x) {
    return(interval.mean[as.character(x)])
}
new.activity_data[is.na(new.activity_data$steps), "steps"] <- 
    sapply(new.activity_data[is.na(new.activity_data$steps), "interval"], 
           fill.steps)
```
4. Making a histogram of the total number of steps taken each day and calculating and reporting the mean and median total number of steps taken per day using the new 
dataset.

```r
result <- tapply(new.activity_data$steps, new.activity_data$date, sum, 
                 na.rm = T)
barplot(result, main = "Total number of steps", xlab = "Days", ylab = "Steps",
        names.arg = "")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-18-1.png" title="" alt="" width="672" />

```r
steps.mean <- mean(result, na.rm = T)
steps.median <- median(result, na.rm = T)
steps.mean
```

```
## [1] 10766.19
```

```r
steps.median
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?
1.Creating a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
new.activity_data$day <- ifelse(weekdays(as.Date(new.activity_data$date), 
                                         abbreviate = T) %in% c("Sat", "Sun"), 
                                "weekend", "weekday")
new.activity_data <- transform(new.activity_data, day = factor(day))
```

2.Making a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or 
weekend days (y-axis).



