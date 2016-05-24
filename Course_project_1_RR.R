# Course_Project_1_RR                                       May 23 2016
# Code for Assignment Course Project 1 of Reproducible Research, Week 2

interv_max <- function(means) {
    # Find the 5-minute interval that, on average, contains the maximum number of steps
    max_steps <- max(means) # Maximum number of steps in average
    max_5min <- activity$interval[which.max(means)[1]] # Beginning hours-minutes packed in decimal
    hmx <- max_5min %/% 100; mmx <- max_5min %% 100 # Unpack it
    mmx1 <- (mmx + 5) %% 60; hmx1 <- hmx + (mmx + 5) %/% 60 # Interval's end, unpacked 
    # Calculate the plot's abscise corresponding to the left end of that interval:
    # max_abcs <- uniroot(function(x){40*((x-1) %/% 12)+5*(x-1)-max_5min},interval=c(0,n_5min))[1]
    max_abcs <- grep(max_5min, activity$interval, fixed=TRUE)[1]
    return(c(max_steps,hmx,mmx,hmx1,mmx1,max_abcs))
 }

library(dplyr)
library(lubridate)
library(Hmisc)
# Loading and preprocessing the data:
# Code for reading in the dataset and processing the data
if (length(grep("activity",ls(),fixed = TRUE))==0) {
    activity <- read.csv("./data/activity.csv", header=TRUE)
}

# Processing the original data:
# =============================
# Compute global parameters:
ndays <- length(levels(activity$date)) # number of days
nobs <- nrow(activity)                 # number of observations
l_int <- activity[2,3]-activity[1,3]   # length of one interval, in minutes
n_5min <- 60*24 / l_int                # number of intervals per day
print("Original activity data frame:")
print(summary(activity))

# Calculate and report the total number of missing values in the dataset 
print(sprintf("Number of missing values: %g",sum(is.na(activity$steps))))
perc_na <- 100*mean(is.na(activity$steps))
print(sprintf("Percentage of missing values: %.3f %%",perc_na))

# ==================================================================================
# Esta sección no corresponde a lo que pide el proyecto: Lo hice con los promedios
# acumulados hasta el momento, y lo que corresponde es usar los promedios por inter-
# valo de 5 minutos. Así que la dejo aquí pero no va a ir al R Markdownn
# ===================================================================================
# //////////////////////////////////  NO VA:  ////////////////////////////////////
# # Time series plot of the average number of steps taken:
# ave_steps <- matrix(0, nrow = 1, ncol = ndays)
# # Calculate the total number of steps taken per day:
# total_daily_steps <- sapply(split(activity, activity$date),function(x) {sum(x$steps, na.rm = TRUE)})
# # Calculate the average number of steps per day:
# sum_steps <- 0
# for (iday in 1:ndays){
#     sum_steps <- sum_steps + total_daily_steps[iday]
#     ave_steps[iday] <- sum_steps/iday
# }
# 
# par(mfrow = c(1,1))
# plot(1:ndays,ave_steps, type = 'l',lwd = 2, col="blue",
#       main="Time series plot of the average number of steps taken",
#       xlab="Day's number",
#       ylab="Average number of steps")
# minor.tick(nx=10, tick.ratio=0.6)
# grid()
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ HASTA AQUI NO VA \\\\\\\\\\\\\\\\\\\\\\\\\\\\
# ==================================================================================
# Time series plot of the average number of steps taken (by 5-minutes interval):
# Calculate the total number of steps taken per day:
total_daily_steps <- sapply(split(activity, activity$date),function(x) {sum(x$steps, na.rm = TRUE)})
# Find and print the mean:
mean_total <- mean(total_daily_steps)
print(sprintf("Mean total number of steps taken per day: %.2f",mean_total))
# Create a vector to keep the fancy expression of intervals:
inter_5min <- matrix(0,nrow = 1,ncol = n_5min)
nin <- 0
for (interv in activity$interval[1:n_5min]){
    nin <- nin + 1
    hora <- interv %/% 100; minutos <- interv %% 100
    inter_5min[nin] <- paste(as.character(hora),substr(as.character(minutos+100),2,3),sep=":")
}
# Find the average number of steps taken per 5-minute interval:
five_min_means <- with(activity,
            sapply(split(activity,interval),function(x) {mean(x$steps, na.rm = TRUE)}))
# Find the 5-minute interval that, on average, contains the maximum number of steps
answ <- interv_max(five_min_means) # Function to do some interval computations
mmx5 <- answ[[1]];hmx <- answ[[2]];mmx <- answ[[3]];hmx1 <- answ[[4]];mmx1 <- answ[[5]]
max_abcs <- answ[[6]]
print(sprintf("5-minute interval with the average maximum number of steps: %d:%2d - %d:%2d"
              ,hmx,mmx,hmx1,mmx1))

#Draw the time series plot:
par(mfrow = c(1,1))
plot(1:n_5min,five_min_means, type='l',col='blue',xaxt = "n",ylim = c(0,250),
     main="Average number of steps taken per 5-minute interval",
     xlab="Time series along a day sampled in 5-minute intervals",
     ylab="Average number of steps",
     cex.axis=0.8, cex.lab=0.8)
axis(1,labels=inter_5min[seq(1,n_5min,6)],at=seq(0,n_5min-1,6), cex.axis=0.8, cex.lab=0.8)
text(x=260,y=210,paste("Number of observations:",as.character(nobs)),cex=0.7)
text(x=260,y=200,sprintf("Max.average of steps taken:%.2f",mmx5),cex=0.7)
abline(v = max_abcs, col ="red")
# Save file
dev.copy(png, file="./graphs/TimeSeries1.png")
dev.off()

# HISTOGRAMS:
par(mfrow = c(2,1), mar=c(4,4,2,2))
# Histogram of the total number of steps taken each day:
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

# Report the mean and median of the total number of steps taken per day: 
print(sprintf("Median number of steps by day: %.2f",median(total_daily_steps)))
print(sprintf("Mean number of steps by day: %.2f",mean(total_daily_steps)))

# Devise a strategy for filling in all of the missing values in the dataset.
# Code to describe and show a strategy for imputing missing data
imputed_activity <- activity  # Initializing new dataset imputed_activity:
if (perc_na > 0) {
    # Imputing values for NA's: I prefer to use the mean of each 5 minutes interval:
    # Create a new dataset that is equal to the original dataset but with the missing data filled in.
    # Impute the mean to intervals where there are a missing value:
    for (irow in 1:nrow(activity)) { 
        n <- (irow-1) %% n_5min + 1
        if (is.na(activity[irow,1])) {imputed_activity[irow,1] <- five_min_means[n]}
    }
    # With imputed values:
    print("====================================================================================== ")
    print("Imputed activity data frame:")
    print(summary(imputed_activity))
    print(sprintf("After imputing, missing values:%.3f %%",100*mean(is.na(imputed_activity$steps))))
    
    # Histogram of the total number of steps taken each day after missing values are imputed:
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
    lines(c(14000,16000),c(14.5,14.5), col="red", lwd=2) 
    text(x=18300, y=14.5,"median = mean")    # Explains drawn lines
    
    # Report the mean and median of the total number of steps taken per day in the imputed data: 
    print(sprintf("Median number of steps by day: %.2f",median(total_daily_steps)))
    print(sprintf("Mean number of steps by day: %.2f",mean(total_daily_steps))) 
    # Imputing missing data had an impact on the estimates of the total daily number of steps:
    # they have grown, and so are the median and mean, which, because of the chosen imputing
    # strategy, have furthermore unified their value. The median has grown in 3.57% and the mean
    # in 15.09%. 
}
# Save file
dev.copy(png, file="./graphs/Histog.png", height=540, width=720)
dev.off()
# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and 
# weekends.
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

#Draw the two paneled plots:
par(mfrow = c(2,1), oma=c(1,0,3,2), mar=c(4,4,2,2))
plot(1:n_5min,prom_wd, type='l',col='blue',xaxt = "n",ylim = c(0,250),
     main="On weekdays",
     xlab="",
     ylab="Average number of steps",
     cex.axis=0.8, cex.lab=0.8, cex.main=0.95)
axis(1,labels=inter_5min[seq(1,n_5min,6)],at=seq(0,n_5min-1,6), cex.axis=0.8, cex.lab=0.8)
text(x=260,y=210,paste("Number of observations:",as.character(nwd)),cex=0.7)
text(x=260,y=190,sprintf("Max.average of steps taken:%.2f",mmx5_wd),cex=0.7)
abline(v = max_abcs_wd, col ="red")
title("Average number of steps taken per 5-minute interval", outer=TRUE)

plot(1:n_5min,prom_we, type='l',col='blue',xaxt = "n",ylim = c(0,250),
     main="On weekends",
     xlab="Time series along a day sampled in 5-minute intervals",
     ylab="Average number of steps",
     cex.axis=0.8, cex.lab=0.8, cex.main=0.95)
axis(1,labels=inter_5min[seq(1,n_5min,6)],at=seq(0,n_5min-1,6), cex.axis=0.8, cex.lab=0.8)
text(x=260,y=210,paste("Number of observations:",as.character(nwe)),cex=0.7)
text(x=260,y=190,sprintf("Max.average of steps taken:%.2f",mmx5_we),cex=0.7)
abline(v = max_abcs_we, col ="red")
# Save file
dev.copy(png, file="./graphs/TimeSeries2.png", height=540, width=720)
dev.off()

# Conclusions about activity patterns changes seen in these two graphs:
# - Activity begins two hours earlier on weekdays (6:00 vs. 8:00)
# - Maximum activity is greater, and achieved earlier, on weekdays (8:35, 230 steps vs. 9:15, 167 steps)
# - After the peak morning activity, weekends show more physical activity than weekdays.
# - Could be said that the subject is a white-collar worker.