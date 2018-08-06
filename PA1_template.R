library(ggplot2)
library(scales)
library(Hmisc)


activity = read.csv("//Users/user/Documents/KrisTan/Coursera/activity.csv")
summary(activity)

#activity$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activity$interval), format='%H:%M')

stepsByDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)

qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)

averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 

mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])

numMissingValues <- length(which(is.na(activity$steps))

activityImputed <- activity

activityImputed$steps <- impute(activity$steps, fun=mean)

stepsByDayImputed <- tapply(activityImputed$steps, activityImputed$date, sum)

qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)

stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)

activityImputed$dateType <-  ifelse(as.POSIXlt(activityImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

averagedActivityImputed <- aggregate(steps ~ interval + dateType, data=activityImputed, mean)
ggplot(averagedActivityImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")