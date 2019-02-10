
## Loading the file 
library(readr)
library(ggplot2)
library(dplyr)
library(lattice)
activity <- read_csv("activity.csv")
str(activity)
summary(activity)
head(activity)

## Number of steps taken per day
activitynoNA <- na.omit(activity)
totalsteps <- aggregate(steps ~ date, activitynoNA, sum)

## Histogram 
ggplot(totalsteps, aes (steps)) +
  geom_histogram(fill="Navy", bins = 45) +
  labs(title = "Daily Steps Histogram", x="Steps", y="Frequency") 

## Median and Mean
mean(totalsteps$steps)
median(totalsteps$steps)

## By interval 
intsteps <- aggregate(steps ~ interval, activitynoNA, mean)

## Plot
ggplot(intsteps, aes (interval,steps)) +
  geom_line(color="Orange") +
  labs(title = "Average Number of Steps per Day by Interval", x="Interval", y="Steps") 

## Max Steps 
intsteps$interval[which.max(intsteps$steps)]

## How many missing values we have?
sum(is.na(activity))

##Replacing missing values 
NAindex <- which(is.na(as.character(activity$steps)))
completeactivity <- activity
completeactivity[NAindex, ]$steps<-unlist(lapply(NAindex, FUN=function(NAindex){
  intsteps[activity[NAindex,]$interval==intsteps$interval,]$steps
}))

##Generating histogram
totalstepscomplete <- aggregate(steps ~ date, completeactivity, sum)
ggplot(totalstepscomplete, aes (steps)) +
  geom_histogram(fill="Red", bins = 45) +
  labs(title = "Daily Steps Histogram", x="Steps", y="Frequency") 

## Median and Mean
mean(totalstepscomplete$steps)
median(totalstepscomplete$steps)

## How do they differ
mean(totalstepscomplete$steps) - mean(totalsteps$steps)
 
median(totalstepscomplete$steps) - median(totalsteps$steps) 

## Creating new variables.
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
completeactivity$dayofweek = as.factor(ifelse(is.element(weekdays(completeactivity$date),weekdays), "Weekday", "Weekend"))
stepstotalwkd <- aggregate(steps ~ interval + dayofweek, completeactivity, mean)

## Creating Final plot 
xyplot(stepstotalwkd$steps ~ stepstotalwkd$interval|stepstotalwkd$dayofweek, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")


