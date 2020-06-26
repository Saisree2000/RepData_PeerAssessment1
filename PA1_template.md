---
title: "Reproducible Research: Peer Assessment 1"
author: "Sai Sree"
date: "26/06/2020"
output: html_document
---

## Loading and preprocessing the data

```{r}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "Project-1")
```
```{r, echo=TRUE}
activity <- read.csv("Project-1/activity.csv",header=TRUE)
activity$date <- as.POSIXct(activity$date)
```
## What is mean total number of steps taken per day?  
```{r, echo=TRUE}
stepsdaily <- tapply(activity$steps,activity$date,sum,na.rm=TRUE)
hist(stepsdaily,xlab="daily number of steps",ylab="Frequency")
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()
mean.steps <- mean(stepsdaily)
median.steps <- median(stepsdaily)
```
Hence, mean of total number of steps taken per day is `r mean.steps`
and median is `r median.steps`
## What is the average daily activity pattern?  
```{r, echo=TRUE}
avgsteps <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm=TRUE)
med<- aggregate(activity$steps, by = list(activity$interval), median, na.rm=TRUE)
names(med) = c("interval","median_steps")
names(avgsteps) = c("interval","average_steps")
library(ggplot2)
ggplot(avgsteps, aes(x = interval, y = average_steps)) + geom_line()
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()
m <- avgsteps$interval[avgsteps$average_steps == max(avgsteps$average_steps)]
```
`r m` is the 5-minute interval, on average across all the days in the dataset which contains the maximum number of steps.

## Imputing missing values    
```{r, echo=TRUE}
s <- subset(activity,is.na(activity$steps))
n <- nrow(is.na(s))
```
No.of rows with NAs in the data set=`r n`
```{r, echo=TRUE}
nstps <- data.frame(date=activity$date[is.na(activity$steps)], interval = activity$interval[is.na(activity$steps)], steps=med[match(med$interval, activity$interval[is.na(activity$steps)]),2])
activity <- subset(activity, !is.na(steps))
activity <- rbind(activity, nstps)
stepsdaily2 <- aggregate(activity$steps, by = list(activity$date), sum, na.rm=TRUE)
names(stepsdaily2) <- c("Date", "steps")
hist(stepsdaily2$steps,xlab="Daily number of steps", main="Histogram of stepsdaily2")
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()
```
```{r, echo=TRUE}
mean.steps2 <- mean(stepsdaily2$steps)
median.steps2 <- median(stepsdaily2$steps)
dm<-sum(-mean.steps,mean.steps2)
dn<-sum(median.steps2,-median.steps)
```
The new mean is `r mean.steps2` and the old mean is `r mean.steps`, their difference is `r dm`.The new median is `r median.steps2`and the old median is `r median.steps`, difference between them is `r dn`.The impact of using the median for the time interval to replace the missing data seems to be very less.

## Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
activity$week <-ifelse(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday" ,"weekend","weekday")
avgsteps2 <- aggregate(activity$steps, by = list(activity$week, activity$interval), mean, na.rm=TRUE)
med2 <- aggregate(activity$steps, by = list(activity$week, activity$interval), median, na.rm=TRUE)
avgsteps2 <- cbind(avgsteps2[], med2$x)

names(avgsteps2) = c("weekday", "interval","mean.steps", "median.steps")
avgsteps2$mean.steps <- round(avgsteps2$mean.steps)
avgsteps2$median.steps <- round(avgsteps2$median.steps)
ggplot(avgsteps2, aes(x = interval, y = mean.steps,col=weekday)) + geom_line() + facet_wrap(~weekday , ncol = 1, nrow=2)
dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
```


