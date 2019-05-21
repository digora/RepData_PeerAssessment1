#Test Scripts
##Solution Environment
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)

## Loading and preprocessing the data
#Set the working directory assuming you cloned the repository directly onto your home directory
setwd("~/RepData_PA1/activity")

#Extract dataset as Data Table
mad <- fread("activity.csv")

#Preprocessing

## What is mean total number of steps taken per day?

#Calculate the total number of steps taken per day
by_date <- group_by(mad, date)
madTotalSteps <- summarise(by_date, totStepsPerDay = sum(steps, na.rm = FALSE))

#Make a histogram of the total number of steps taken each day
hist(madTotalSteps$totStepsPerDay, main="Daily Steps Frequency", xlab="Total Steps", breaks=nrow(madTotalSteps))

#Calculate and report the mean and median of the total number of steps taken per day
madMeanSteps <- summarise(by_date, meanSteps = mean(steps, na.rm = TRUE))
madMedSteps <- summarise(by_date, medSteps = median(steps, na.rm = TRUE))

dev.off()

## What is the average daily activity pattern?
#Make a time series plot (i.e.l) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
by_int <- group_by(mad, interval)
madMeanIntSteps <- summarise(by_int, intMeanSteps = mean(steps, na.rm=TRUE))

ggplot(madMeanIntSteps, aes(x = interval , y = intMeanSteps)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")

dev.off()

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxInt <- arrange(madMeanIntSteps, desc(intMeanSteps)) %>% slice(1)

## Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(mad[is.na(steps),])

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Use the mean to fill in the NA values in the dataset
mad[is.na(steps), "steps"] <- mad[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps")]

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
data.table::fwrite(x = mad, file = "tidyActivity.csv", quote = FALSE)

#Extract the clean dataset
madClean <- fread("tidyActivity.csv")

#Make a histogram of the total number of steps taken each day and 
by_date <- group_by(madClean, date)
mcTotalSteps <- summarise(by_date, totStepsPerDay = sum(steps, na.rm = FALSE))
hist(mcTotalSteps$totStepsPerDay, main="Daily Steps Frequency", xlab="Total Steps", breaks=nrow(mcTotalSteps))

#Calculate and report the mean and median total number of steps taken per day. 
mcMeanTotSteps <- mean(mcTotalSteps$totStepsPerDay, na.rm = FALSE)
mcMedTotSteps <- median(mcTotalSteps$totStepsPerDay, na.rm = FALSE)

#Do these values differ from the estimates from the first part of the assignment?
madMeanTotSteps <- mean(madTotalSteps$totStepsPerDay, na.rm = TRUE)
madMedianTotSteps <- median(madTotalSteps$totStepsPerDay, na.rm = TRUE)

mmLabel <- c("Mean", "Median")
oldMM <- c(madMeanTotSteps, madMedianTotSteps)
newMM <- c(mcMeanTotSteps, mcMedTotSteps)

mmComparison <- data.table(mmLabel, oldMM, newMM)

##It can be seen that the values do differ after inputting the missing variables as Mean
View(mmComparison)

#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#It increases the frequency of steps per day

dev.off()

## Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels – 
#“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
mad[, date := as.POSIXct(date, format = "%Y-%m-%d")]
mad[, `Day of Week`:= weekdays(x = date)]
mad[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
mad[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
mad[, `weekday or weekend` := as.factor(`weekday or weekend`)]

#Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should 
#look like using simulated data.
mad[is.na(steps), "steps"] <- mad[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
madInterval <- mad[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(madInterval , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)

dev.off()
