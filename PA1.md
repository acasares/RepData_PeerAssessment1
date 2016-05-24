# Reproducible Research: Peer Assessment 1


## Introduction
It is now possible to collect a large amount of data about personal
movement using activity monitoring devices.

These type of devices are part of the "quantified self" movement -- 
a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment has been downloaded from the course web
site.


The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## A required function

```r
interv_max <- function(means) {
    # Find the 5-minute interval that, on average, contains the maximum number of steps
    max_steps <- max(means) # Maximum number of steps in average
    max_5min <- activity$interval[which.max(means)[1]] # Beginning hours-minutes packed in decimal
    hmx <- max_5min %/% 100; mmx <- max_5min %% 100 # Unpack it
    mmx1 <- (mmx + 5) %% 60; hmx1 <- hmx + (mmx + 5) %/% 60 # Interval's end, unpacked 
    # Calculate the plot's abscise corresponding to the left end of that interval:
    # Could be found by solving an equation:
    # max_abcs <- uniroot(function(x){40*((x-1) %/% 12)+5*(x-1)-max_5min},interval=c(0,n_5min))[1]
    # Or can be retrieved from the activity data frame:
    max_abcs <- grep(max_5min, activity$interval, fixed=TRUE)[1]
    return(c(max_steps,hmx,mmx,hmx1,mmx1,max_abcs))
 }
```
## Some libraries loaded:

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Warning: package 'lubridate' was built under R version 3.2.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```
## Warning: package 'Hmisc' was built under R version 3.2.5
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Warning: package 'survival' was built under R version 3.2.5
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
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

## Loading and preprocessing the data

Code for reading in the dataset and processing the data.
The reading is done only if the data is not already loaded in memory:


```r
if (length(grep("activity",ls(),fixed = TRUE))==0) {
    activity <- read.csv("./data/activity.csv", header=TRUE)
}
```
Processing the original data:
Compute global parameters:

```r
ndays <- length(levels(activity$date)) # number of days
nobs <- nrow(activity)                 # number of observations
l_int <- activity[2,3]-activity[1,3]   # length of one interval, in minutes
n_5min <- 60*24 / l_int                # number of intervals per day
print("Original activity data frame:")
```

```
## [1] "Original activity data frame:"
```

```r
print(summary(activity))
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
## What is "mean total number of steps taken per day"?
It is the average sum of the number of steps taken during the 288 five-minute intervals of a day.
To calculate it, first find the total daily steps, using the paradigm split (activity per date) + sapply, with the function sum. On this result will be based the histograms to be built later.

```r
total_daily_steps <- sapply(split(activity, activity$date),function(x) {sum(x$steps, na.rm = TRUE)})
```
Then take the mean and report it an the median:

```r
mean_total <- mean(total_daily_steps)
print(sprintf("Mean total number of steps taken per day: %.2f",mean_total))
```

```
## [1] "Mean total number of steps taken per day: 9354.23"
```

```r
print(sprintf("Median total number of steps taken per day: %.2f",median(total_daily_steps)))
```

```
## [1] "Median total number of steps taken per day: 10395.00"
```
Now is as good a time as any other to create a vector with the fancy expression of intervals, to be used on the plots:

```r
inter_5min <- matrix(0,nrow = 1,ncol = n_5min)
nin <- 0
for (interv in activity$interval[1:n_5min]){
    nin <- nin + 1
    hora <- interv %/% 100; minutos <- interv %% 100
    inter_5min[nin] <- paste(as.character(hora),substr(as.character(minutos+100),2,3),sep=":")
}
```
## What is the average daily activity pattern?
It is a time series along a day, showing the average number of steps taken on each five minute interval

Find the average number of steps taken per 5-minute interval:

```r
five_min_means <- with(activity,
            sapply(split(activity,interval),function(x) {mean(x$steps, na.rm = TRUE)}))
```
Find the 5-minute interval that, on average, contains the maximum number of steps

```r
answ <- interv_max(five_min_means) # Function to do some interval computations
mmx5 <- answ[[1]];hmx <- answ[[2]];mmx <- answ[[3]];hmx1 <- answ[[4]];mmx1 <- answ[[5]]
max_abcs <- answ[[6]]
print(sprintf("5-minute interval with the average maximum number of steps: %d:%2d - %d:%2d"
              ,hmx,mmx,hmx1,mmx1))
```

```
## [1] "5-minute interval with the average maximum number of steps: 8:35 - 8:40"
```
## Time series plot of the average number of steps taken
It's assumed that the average is on the number of steps by five-minute interval, along a day

```r
    #Draw the time series plot:
    par(mfrow = c(1,1))
    plot(1:n_5min,five_min_means, type='l',col='blue',xaxt = "n",ylim = c(0,250),
         main="Average number of steps taken per 5-minute interval",
         xlab="Time series along a day sampled in 5-minute intervals",
         ylab="Average number of steps",
         cex.axis=0.8, cex.lab=0.8)
    axis(1,labels=inter_5min[seq(1,n_5min,6)],at=seq(0,n_5min-1,6), cex.axis=0.8, cex.lab=0.8)
    text(x=240,y=210,paste("Number of observations:",as.character(nobs)),cex=0.7)
    text(x=240,y=200,sprintf("Max.average of steps taken:%.2f",mmx5),cex=0.7)
    abline(v = max_abcs, col ="red")
```

![](PA1_files/figure-html/plot1-1.png)<!-- -->

## Imputing missing values

Calculate and report the total number of missing values in the dataset 

```r
print(sprintf("Number of missing values: %g",sum(is.na(activity$steps))))
```

```
## [1] "Number of missing values: 2304"
```

```r
perc_na <- 100*mean(is.na(activity$steps))
print(sprintf("Percentage of missing values: %.3f %%",perc_na))
```

```
## [1] "Percentage of missing values: 13.115 %"
```
Devise a strategy for filling in all of the missing values in the dataset.

Code to describe and show a strategy for imputing missing data:

```r
imputed_activity <- activity  # Initializing new dataset imputed_activity:
# Imputing values for NA's: I prefer to use the mean of each 5 minutes interval:
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Impute the mean to intervals where there are a missing value:
for (irow in 1:nrow(activity)) { 
    n <- (irow-1) %% n_5min + 1
    if (is.na(activity[irow,1])) {imputed_activity[irow,1] <- five_min_means[n]}
}
print("Imputed activity data frame:")
```

```
## [1] "Imputed activity data frame:"
```

```r
print(summary(imputed_activity))
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

```r
# A verification:
print(sprintf("After imputing, missing values:%.3f %%",100*mean(is.na(imputed_activity$steps))))
```

```
## [1] "After imputing, missing values:0.000 %"
```
Recalculate total number of daily steps:

```r
total_daily_steps1 <- sapply(split(imputed_activity, imputed_activity$date),
                            function(x) {sum(x$steps, na.rm = TRUE)})

    # Report the mean and median of the total number of steps taken per day in the imputed data: 
print(sprintf("Median number of steps by day: %.2f",median(total_daily_steps1)))
```

```
## [1] "Median number of steps by day: 10766.19"
```

```r
print(sprintf("Mean number of steps by day: %.2f",mean(total_daily_steps1))) 
```

```
## [1] "Mean number of steps by day: 10766.19"
```
Imputing missing data had an impact on the estimates of the total daily number of steps:

They have grown, and so are the median and mean, which, because of the chosen imputing
strategy, have furthermore unified their value. The median has grown in 3.57% and the mean
in 15.09%.

## Plot two paneled histograms, with original and imputed data sets, respectively:

```r
# HISTOGRAMS:
par(mfrow = c(2,1), oma=c(1,0,3,2), mar=c(4,4,2,2))
# 1.-Histogram of the total number of steps taken each day:
# Drawn as first plot in a panel of two
hist(total_daily_steps, col = "green", breaks = 20, 
     main = "Activity with missing values",
     xlab = "",
     cex.main = 0.95)
rug(total_daily_steps)
abline(v = mean(total_daily_steps), col = "black")
abline(v = median(total_daily_steps), col = "red", lwd=2)
lines(c(15000,17000),c(8.5,8.5), col="red", lwd=2);lines(c(15000,17000),c(7,7), col="black")
text(x=18400, y=8.5,"median");text(x=18400, y=7,"mean")
title("Histogram of total daily steps.", outer=TRUE) 

# 2.-Histogram of the total number of steps taken each day after missing values are imputed:
total_daily_steps <- sapply(split(imputed_activity, imputed_activity$date),
                            function(x) {sum(x$steps, na.rm = TRUE)}) # Evaluate daily totals
# Drawn as second plot in a panel of two
hist(total_daily_steps, col = "green", breaks = 20, 
     main = "Activity without missing values",
     cex.main = 0.95)
rug(total_daily_steps)
# Draw in the plot vertical lines marking mean and median:
abline(v = mean(total_daily_steps), col = "black")
abline(v = median(total_daily_steps), col = "red", lwd=2)
lines(c(14000,15500),c(14.5,14.5), col="red", lwd=2) 
text(x=18300, y=14.5,"median = mean")    # Explains drawn lines
```

![](PA1_files/figure-html/histog-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

I rather made two data frames, one for weekdays and another for weekends, instead of splitting
the imputed data frame by day of the week and then by interval: it is clearer so.

```r
# Create a new data frame with a new column: day of the week
activity1 <- mutate(imputed_activity, week_day=wday(ymd(imputed_activity$date)))
# According to suggestion in instructions:
activity1 <- mutate(activity1, week_day=2-(week_day > 1 & week_day < 7)) # weekday:1, weekend:2 

# Filter the data frame splitting it by labor days and holidays, recording the number of
# observations in each:
act_wd <- filter(activity1, activity1$week_day == 1) 
nwd <- dim(act_wd[1])[1]
act_we <- filter(activity1, activity1$week_day == 2)
nwe <- dim(act_we[1])[1]

# FOR WEEK DAYS
# Find the average number of steps taken per 5-minute interval over weekdays:
prom_wd <- sapply(split(act_wd, act_wd$interval),function(x) {
    mean(x$steps, na.rm = TRUE)})

# Find the 5-minute interval that, on average, contains the maximum number of steps
answ <- interv_max(prom_wd)
mmx5_wd <- answ[[1]];hmx_wd <- answ[[2]];mmx_wd <- answ[[3]];hmx1_wd <- answ[[4]];mmx1_wd <- answ[[5]]
max_abcs_wd <- answ[[6]]
print(sprintf("5-minute interval with the average maximum number of steps in week days: %d:%2d - %d:%2d"
              ,hmx_wd,mmx_wd,hmx1_wd,mmx1_wd))
```

```
## [1] "5-minute interval with the average maximum number of steps in week days: 8:35 - 8:40"
```

```r
# FOR WEEKENDS
# Find the average number of steps taken per 5-minute interval over weekends:
prom_we <- sapply(split(act_we, act_we$interval),function(x) {
    mean(x$steps, na.rm = TRUE)})

# Find the 5-minute interval that, on average, contains the maximum number of steps
answ <- interv_max(prom_we)
mmx5_we <- answ[[1]];hmx_we <- answ[[2]];mmx_we <- answ[[3]];hmx1_we <- answ[[4]];mmx1_we <- answ[[5]]
max_abcs_we <- answ[[6]]
print(sprintf("5-minute interval with the average maximum number of steps in weekends: %d:%2d - %d:%2d"
              ,hmx_we,mmx_we,hmx1_we,mmx1_we))
```

```
## [1] "5-minute interval with the average maximum number of steps in weekends: 9:15 - 9:20"
```

```r
#Draw the two paneled plots:
par(mfrow = c(2,1), oma=c(1,0,3,2), mar=c(4,4,2,2))
plot(1:n_5min,prom_wd, type='l',col='blue',xaxt = "n",ylim = c(0,250),
     main="On weekdays",
     xlab="",
     ylab="Average number of steps",
     cex.axis=0.8, cex.lab=0.8, cex.main=0.95)
axis(1,labels=inter_5min[seq(1,n_5min,6)],at=seq(0,n_5min-1,6), cex.axis=0.8, cex.lab=0.8)
text(x=200,y=240,paste("Number of observations:",as.character(nwd)),cex=0.7)
text(x=200,y=190,sprintf("Max.average of steps taken:%.2f",mmx5_wd),cex=0.7)
abline(v = max_abcs_wd, col ="red")
title("Average number of steps taken per 5-minute interval", outer=TRUE)

plot(1:n_5min,prom_we, type='l',col='blue',xaxt = "n",ylim = c(0,250),
     main="On weekends",
     xlab="Time series along a day sampled in 5-minute intervals",
     ylab="Average number of steps",
     cex.axis=0.8, cex.lab=0.8, cex.main=0.95)
axis(1,labels=inter_5min[seq(1,n_5min,6)],at=seq(0,n_5min-1,6), cex.axis=0.8, cex.lab=0.8)
text(x=200,y=240,paste("Number of observations:",as.character(nwe)),cex=0.7)
text(x=200,y=190,sprintf("Max.average of steps taken:%.2f",mmx5_we),cex=0.7)
abline(v = max_abcs_we, col ="red")
```

![](PA1_files/figure-html/patterns-1.png)<!-- -->

        (The x axis marks in the previous plots are drawn at half hour intervals).

Yes, there are differences. Some conclusions have been extracted from the graphs:

- Activity begins two hours earlier on weekdays (6:00 vs. 8:00)
- Maximum activity is greater, and achieved earlier, on weekdays (8:35, 230 steps vs. 9:15, 167 steps)
- After the peak morning activity, weekends show more physical activity than weekdays.
- Could be said that the subject is a white-collar worker.
