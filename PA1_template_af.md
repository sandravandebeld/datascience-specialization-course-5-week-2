---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data.

```r
wd <- getwd()
wd
```

```
## [1] "C:/Users/Administrator/data_science_specialisation/R directory/course 5/week 2"
```

```r
library(readr)
activity <- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
### Calculate the total number of steps taken per day.

```r
steps_day <- aggregate(steps ~ date, activity, sum)
head(steps_day)
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
### Make a histogram of the total number of steps taken each day

```r
hist(steps_day$steps)
```

![](PA1_template_af_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


### Calculate and report the mean and median of the total number of steps taken per day

```r
steps <- steps_day[,2]
mean(steps)
```

```
## [1] 10766.19
```

```r
median(steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_interval <- aggregate(steps ~ interval, activity, mean)
plot(steps_interval$interval, steps_interval$steps, type = "l", col = "red")
```

![](PA1_template_af_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
### What is the interval with the max average steps

```r
head(steps_interval[order(steps_interval$steps, decreasing = TRUE),],1)
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```


### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
steps_complete <- impute(activity$steps, mean)
activity_complete <- cbind(activity[,2:3], steps_complete)
colSums(is.na(activity_complete))
```

```
##           date       interval steps_complete 
##              0              0              0
```


### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_day_complete <- aggregate(steps_complete ~ date, activity_complete, sum)
hist(steps_day_complete$steps_complete)
```

![](PA1_template_af_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
steps_complete <- steps_day_complete[,2]
mean(steps_complete)
```

```
## [1] 10766.19
```

```r
median(steps_complete)
```

```
## [1] 10766.19
```

#### uitkomst is hetzelfde

## Are there differences in activity patterns between weekdays and weekends?

```r
weekday <- weekdays.Date(activity_complete$date)
df1 <- cbind(activity_complete, weekday)
week <- ifelse(weekday %in% c("zaterdag", "zondag"), "weekend", "weekday")
df2 <- cbind (df1, week)
summary(df2)
```

```
## 
##  2304 values imputed to 37.3826
```

```
##       date               interval      steps_complete        weekday    
##  Min.   :2012-10-01   Min.   :   0.0   Min.   :  0.00   dinsdag  :2592  
##  1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:  0.00   donderdag:2592  
##  Median :2012-10-31   Median :1177.5   Median :  0.00   maandag  :2592  
##  Mean   :2012-10-31   Mean   :1177.5   Mean   : 37.38   vrijdag  :2592  
##  3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.: 37.38   woensdag :2592  
##  Max.   :2012-11-30   Max.   :2355.0   Max.   :806.00   zaterdag :2304  
##                                                         zondag   :2304  
##       week      
##  weekday:12960  
##  weekend: 4608  
##                 
##                 
##                 
##                 
## 
```

```r
df_weekday <- subset.data.frame(df2, week ==  "weekday")
steps_interval_weekday <- aggregate(steps_complete ~ interval, df_weekday, mean)
plot(steps_interval_weekday$interval, steps_interval_weekday$steps_complete, type = "l", col = "red", main = "activity weekday")
```

![](PA1_template_af_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
df_weekend <- subset.data.frame(activity_complete, week ==  "weekend")
steps_interval_weekend <- aggregate(steps_complete ~ interval, df_weekend, mean)
plot(steps_interval_weekend$interval, steps_interval_weekend$steps_complete, type = "l", col = "red", main = "activity weekend")
```

![](PA1_template_af_files/figure-html/unnamed-chunk-10-2.png)<!-- -->


