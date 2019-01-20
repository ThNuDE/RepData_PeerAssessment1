---
title: "Reproducible Research: Peer Assessment 1"
author: "Thomas Nüßlein"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
# libraries
library(ggplot2)
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
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
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
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
length(unique(activity$date))
```

```
## [1] 61
```

```r
summary(activity)
```

```
##      steps             date                        interval     
##  Min.   :  0.00   Min.   :2012-10-01 00:00:00   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16 00:00:00   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31 00:00:00   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-30 23:32:27   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15 00:00:00   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30 00:00:00   Max.   :2355.0  
##  NA's   :2304
```

```r
steps_day <- aggregate(steps ~ date, rm.na = TRUE, data = activity, FUN = sum)
plot(steps_day, type = "h", lwd = 10, lend = "square")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggplot(data = steps_day, aes(x = date, y = steps)) +
  geom_line() +
  geom_point() +
  xlab("Date") +
  ylab("Steps") +
  ggtitle("Histogram of steps per day") +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->


## What is mean total number of steps taken per day?

```r
   rmean <- mean(steps_day$steps, na.rm=TRUE)
   rmedian <- median(steps_day$steps, na.rm=TRUE)
```
The `mean` is 1.0767189\times 10^{4} and the `median` is 10766.

## What is the average daily activity pattern?

```r
   StepsByInterval <- aggregate(steps ~ interval, rm.na = TRUE, data = activity, FUN = mean)
   plot(StepsByInterval, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per     intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
w1 <- which.max(StepsByInterval$steps)   
max_interval <- StepsByInterval[which.max(StepsByInterval$steps),1]
maxSteps <- max(StepsByInterval$steps, na.rm = TRUE)
maxSteps2 <- max(activity$steps, na.rm = TRUE)
head(StepsByInterval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
StepsByInterval[104, 1]
```

```
## [1] 835
```
W1: 104.
Max Interval: 835.
Max Steps: 206.1698113.
Max Steps 2: 806.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
