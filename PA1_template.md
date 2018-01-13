---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

***

## Introduction

This project is in the context of the Reproducible Research Course.

Please see README.md for a full description of data and current assignment

## Libraries used

For this document, the following librarires were loaded:


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.4.3
```

```r
library(lattice)
library(data.table)
```


## Loading and preprocessing the data

Loading of this Dataset was achieved using the *read.csv* function:


```r
x<-read.csv("activity.csv")
head(x)
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

No preprocessing was necessary.


## What is mean total number of steps taken per day?

### Part 1: Histogram of the total number of steps taken each day

For this, I started by creating a new Dataset (*x_new*) that sums all steps taken per day:


```r
xsum<-ddply(x, ~date, summarize, sum=sum(steps, na.rm=T))
```

Then used *xsum* to plot the histogram:


```r
hist(xsum$sum, xlab="Number of steps", ylab="Frequency", main="Histogram of total number of steps each day")
```

![](PA1_template_files/figure-html/plot xsum-1.png)<!-- -->

However, since *na's* are present, *xsum* accounts for them as 0 (zero), which gives some false data and plot.
To correct this, I'll draw just values above 0:


```r
hist(xsum$sum[xsum$sum>0], xlab="Number of steps", ylab="Frequency", main="Histogram of total number of steps each day")
```

![](PA1_template_files/figure-html/plot xsum 2-1.png)<!-- -->

This can be done, because, other that then the *na's* days, all other days have a sum over 0:


```r
aux<-x
aux[is.na(aux)]<-1
xsum_aux <- ddply(aux, ~date, summarize, sum=sum(steps, na.rm=T))
min(xsum_aux$sum)
```

```
## [1] 41
```

By replacing all *na's* with 1, no day has a sum below 41, therefore, no day is incorrectly plotted.

### Part 2: Mean and median total number of steps taken per day

Lets use *xsum* for these 2 calculations:


```r
xmean <- mean(xsum$sum)
xmedian <- median(xsum$sum)
```

Which gives us a mean of 9354.2295082 and a median of 10395.

## What is the average daily activity pattern?

### Part 1: Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

First let's calculate the mean by 5-minute interval:


```r
xmean_interval<-ddply(x, ~interval, summarize, mean=mean(steps, na.rm=T))
```

And now, just plot it:


```r
plot(xmean_interval, type="l", xlab="5-minute interval", ylab="Average steps taken", main="Daily activity pattern")
```

![](PA1_template_files/figure-html/plot xmean_interval-1.png)<!-- -->

### Part 2: Which 5-minute interval, contains the maximum number of steps?

Just by looking at the plot is impossible to day. So lets calculate it:


```r
aux<-xmean_interval[xmean_interval$mean==max(xmean_interval$mean),1]
bux<-floor(xmean_interval[xmean_interval$mean==max(xmean_interval$mean),2])
```

And the winner is interval number 835, with over 206 steps taken!

## Imputing missing values

### Part 1: Total number of missing values in the dataset (i.e. the total number of rows with *NA's*)

We can achieve this by checking which rows aren't complete:


```r
aux<-sum(!complete.cases(x))
```

Which gives us 2304 rows with NA values.

Just do dig a little deeper, and get to know the data set a little better, we could also check where those *NA's* were.

Are they all in the *steps* column?


```r
sum(is.na(x$steps))
```

```
## [1] 2304
```

It checks! All our *NA's* are in the *steps* column.

But even a little deeper, are they full days or are they scattered?


```r
x_na<-x[is.na(x$steps),]
x_na$steps<-1
ddply(x_na, ~date, summarize, sum=sum (steps))
```

```
##         date sum
## 1 2012-10-01 288
## 2 2012-10-08 288
## 3 2012-11-01 288
## 4 2012-11-04 288
## 5 2012-11-09 288
## 6 2012-11-10 288
## 7 2012-11-14 288
## 8 2012-11-30 288
```

8 full days of missing Values, and not one scatered.

### Parts 2 and 3: New dataset with missing values filled in

Let's use a simple strategy of filling missing values with the mean for that 5-minute interval.

Start by creating a new dataset and filling it with the mean calculated above (*xmean_interval*)


```r
x_new <- x
x_new$steps[is.na(x_new$steps)] <- 
    xmean_interval$mean[match(x_new$interval, xmean_interval$interval)][is.na(x_new$steps)]
```

Now, let's calculate the new sum, per day


```r
xsum_new<-ddply(x_new, ~date, summarize, sum=sum(steps, na.rm=T))
```

Finally, lets plot the new histogram. In order to be able to compare them, let's draw them together:


```r
par(mfrow=c(1,2))
hist(xsum$sum[xsum$sum>0], xlab="Number of steps", ylab="Frequency", main="Original Data")
hist(xsum_new$sum, xlab="Number of steps", ylab="Frequency", main="Imputing Missing Values")
```

![](PA1_template_files/figure-html/compare histogram-1.png)<!-- -->

In here, no significant changes are noted.

However, if we consider the first histogram plotted (the one where NA's were counted as 0), 
then differences are quite visible....


```r
par(mfrow=c(1,3))
hist(xsum$sum, xlab="Number of steps", ylab="Frequency", main="Original Data - NA's as 0")
hist(xsum$sum[xsum$sum>0], xlab="Number of steps", ylab="Frequency", main="Original Data - No NA's")
hist(xsum_new$sum, xlab="Number of steps", ylab="Frequency", main="Imputing Missing Values")
```

![](PA1_template_files/figure-html/compare histogram 2-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

### Part 1: Create factor variable with two levels: "weekday" and "weekend"

As instructed, we'll now use *x_new* and add to it, the factor variable:


```r
x_new$day_type<-wday(x_new$date)
x_new$day_type<-as.factor(x_new$day_type)
levels(x_new$day_type) <- list(Weekend=c(1,7), Weekday=c(2:6))
```

### Part 2: Panel plot, time series, 5-minute interval (x-axis) and averaged steps taken (y-axis)

First, let's summarise this data, taking into account both 5-minute interval (*interval*) and wether it's weekday or weekend (*day_type*)


```r
xmean_interval_type<-ddply(x_new, .(interval, day_type), summarise, mean=mean(steps))
```

And finnaly, lets panel plot it:


```r
xyplot(mean ~interval | day_type, 
       data = xmean_interval_type, 
       type="l", 
       layout = c(1,2), 
       xlab="5-Minute Interval", 
       ylab="Average Daily steps")
```

![](PA1_template_files/figure-html/plot xmean_interval_type-1.png)<!-- -->


## That's all folks :)

