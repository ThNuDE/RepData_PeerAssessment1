---
title:   "Reproducible Research: Peer Assessment 1"
author:  "Thomas Nüßlein"
fig_width: 6 
fig_height: 4 
output: 
  html_document:
    keep_md: true
---


## 1.Code for reading in the dataset and/or processing the data
1. Reading the unzipped archive file 'activity.csv'  
2. Loading library ggplot2
3. Display data head
4. Changing date string to date class
5. Display data dimension
6. Display data summary


```r
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
# reading data
activity <- read.csv("activity.csv")

# libraries
library(ggplot2)

# Display data head
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
# Changing date string to date class
activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")

# display dimension
dim(activity)
```

```
## [1] 17568     3
```

```r
# diplay summary
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

### 2.Histogram of the total number of steps taken each day


```r
steps_day <- aggregate(steps ~ date, rm.na = TRUE, data = activity, FUN = sum)
g <- plot(steps_day, type = "h", lwd = 10, lend = "square")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

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

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
knitr::include_graphics("PA1_template_files/figure-html/unnamed-chunk-2-2.png")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->


## 3. Mean and median number of steps taken each day

```r
   rmean <- mean(steps_day$steps, na.rm=TRUE)
   rmedian <- median(steps_day$steps, na.rm=TRUE)
```
The `mean` is 1.0767189\times 10^{4} and the `median` is 10766.

## 4. Time series plot of the average number of steps taken

```r
   StepsByInterval <- aggregate(steps ~ interval, rm.na = TRUE, data = activity, FUN = mean)
   plot(StepsByInterval, type = "l", col="darkblue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per     intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
   knitr::include_graphics("PA1_template_files/figure-html/unnamed-chunk-4-1.png")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 5.The 5-minute interval that, on average, contains the maximum number of steps

```r
row <- which.max(StepsByInterval$steps)   
max_interval <- StepsByInterval[which.max(StepsByInterval$steps),1]
maxSteps <- max(StepsByInterval$steps, na.rm = TRUE)
```
Row: 104.
Max Interval: 835.
Max Steps: 206.1698113.

## 6.Code to describe and show a strategy for imputing missing data
Count the number of days/intervals with missing values (NA).   
  

```r
missing_values = sum(is.na(activity$steps))
```
Calculated number of missing values: 2304
  
Strategy: Create a new dataset and substitute each NA with the overall mean of the variable activity$steps.  

```r
activity_new <- activity
activity_new$steps[is.na(activity_new$steps)] <- mean(na.omit(activity$steps))
steps_day_new <- aggregate(steps ~ date, rm.na = TRUE, data = activity, FUN = sum)
```


## 7.Histogram of the total number of steps taken each day after missing values are imputed

```r
ggplot(data = steps_day_new, aes(x = date, y = steps)) +
  geom_line() +
  geom_point() +
  xlab("Date") +
  ylab("Steps") +
  ggtitle("Histogram of steps per day") +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
 knitr::include_graphics("PA1_template_files/figure-html/unnamed-chunk-8-1.png")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
rmean_new <- mean(steps_day_new$steps, na.rm=TRUE)
rmedian_new <- median(steps_day_new$steps, na.rm=TRUE)
```

The mean without NAs is 1.0767189\times 10^{4} and the median without NAs is 10766.


##8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
# create a new variable with weekdays name
activity_new$day_name <- weekdays(activity_new$date)
# create a new variable indicating weekday or weekend
activity_new$day_type <- ifelse(activity_new$day_name=='Saturday' | activity_new$day_name=='Sunday', 'weekend','weekday')
# check the first 10 values
head(activity_new, n=10)
```

```
##      steps       date interval day_name day_type
## 1  37.3826 2012-10-01        0   Monday  weekday
## 2  37.3826 2012-10-01        5   Monday  weekday
## 3  37.3826 2012-10-01       10   Monday  weekday
## 4  37.3826 2012-10-01       15   Monday  weekday
## 5  37.3826 2012-10-01       20   Monday  weekday
## 6  37.3826 2012-10-01       25   Monday  weekday
## 7  37.3826 2012-10-01       30   Monday  weekday
## 8  37.3826 2012-10-01       35   Monday  weekday
## 9  37.3826 2012-10-01       40   Monday  weekday
## 10 37.3826 2012-10-01       45   Monday  weekday
```

Draw the plot  


```r
steps_day_new <- aggregate(steps~interval+day_type, data=activity_new, FUN=mean, na.action=na.omit)

ggplot(steps_day_new, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average steps per time interval: weekdays/weekends") +
  xlab("Time") +
  ylab("Steps") +
  facet_grid(day_type ~ .) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic", hjust = 0.5),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
   knitr::include_graphics("PA1_template_files/figure-html/unnamed-chunk-10-1.png")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
