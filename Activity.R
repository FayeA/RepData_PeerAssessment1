##########################################
## Loading and preprocessing the data
##

## Unzip the dataset
unzip("activity.zip")

## Read the data
activityData <- read.csv("activity.csv")

## View structure of the activityData
str(activityData)

## Load the dplyr and lattice packages
library(dplyr)
library(lattice)


#####################################################
## What is mean total number of steps taken per day?
##

## 1. 
## Make a histogram of the total number of steps taken each day

## Group the activityData by date variable  
activityByDate <- group_by(activityData, date)

## Create a dataframe with sum of steps per date
stepsByDate <- summarize(activityByDate, totalSteps = sum(steps))

## Create the histogram of total number of steps taken each day.
histogram(stepsByDate$totalSteps)


## 2.
## Calculate and report the mean and median total number of steps taken per day

## Mean of total number of steps taken per day
meanTotalSteps <- mean(stepsByDate$totalSteps, na.rm=TRUE)

## Median of total number of steps taken per day
medianTotalSteps <- median(stepsByDate$totalSteps, na.rm=TRUE)


#####################################################
## What is the average daily activity pattern?
##

## 1. 
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)

## Group the activityData by interval variable  
activityByInterval <- group_by(activityData, interval)

## Create a dataframe with sum of steps per date
stepsByInterval <- summarize(activityByInterval, meanSteps = mean(steps, na.rm=TRUE))

# Create the plot
xyplot(stepsByInterval$meanSteps ~ stepsByInterval$interval, stepsByInterval, type="l")


## 2.
## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?

## Add labels to all points in the plot using the interval variable.
text(x=stepsByInterval$interval, y=stepsByInterval$meanSteps, labels=stepsByInterval$interval, cex=0.7)

## Interval 835 contains the maximum number of steps.


#####################################################
## Imputing missing values
##
## Note that there are a number of days/intervals where there are missing values (coded as NA). 
## The presence of missing days may introduce bias into some calculations or summaries of the data.

## 1. 
## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)

## Calculate total number of rows with missing values.
totalIncompleteRows <- sum(!complete.cases(activityData))

## There are 2304 rows with missing values.


## 2. 
## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## Impute missing values with the mean of the corresponding 5-minute interval. 
## The stepsByInterval data frame, which was previously created, can be used in this imputation strategy.



## 3. Create a new dataset that is equal to the original dataset 
## but with the missing data filled in.

activityDataImputed <- activityData

for(i in 1:nrow(activityDataImputed)){
  if(is.na(activityDataImputed$steps[i])){
    activityDataImputed$steps[i] <- stepsByInterval[stepsByInterval$interval==activityDataImputed[i,3],2]
  }
}


## 4. 
## Make a histogram of the total number of steps taken each day and 
## calculate and report the mean and median total number of steps taken per day. 
##
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Group the activityDataImputed by date variable  
activityByDate_2 <- group_by(activityDataImputed, date)

## Create a dataframe with sum of steps per date
stepsByDate_2<- summarize(activityByDate_2, totalSteps = sum(steps))

## Create the histogram of total number of steps taken each day.
histogram(stepsByDate_2$totalSteps)

## Mean of total number of steps taken per day
meanTotalSteps_2 <- mean(stepsByDate_2$totalSteps, na.rm=FALSE)

## Median of total number of steps taken per day
medianTotalSteps_2 <- median(stepsByDate_2$totalSteps, na.rm=FALSE)


###############################################################################
## Are there differences in activity patterns between weekdays and weekends?
##
## For this part the weekdays() function may be of some help here. 
## Use the dataset with the filled-in missing values for this part.


## 1. 
## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
## indicating whether a given date is a weekday or weekend day.

activityDataImputed <- mutate(activityDataImputed, week=weekdays(as.POSIXct(activityDataImputed$date)))

activityDataImputed <- mutate(activityDataImputed, weekFactor=week)

for(i in 1:nrow(activityDataImputed)){
  if(activityDataImputed[i,4] %in% c("Monday","Tuesday","Wednesday","Thursday", "Friday")){
    activityDataImputed[i,5] <- "weekday"
  }else{
    activityDataImputed[i,5] <- "weekend"
  } 
}

activityDataImputed$weekFactor<- as.factor(activityDataImputed$weekFactor)



## 2. Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis). 
## The plot should look something like the following, which was creating using simulated data:

## Group the activityData by interval variable  
activityByInterval_2 <- group_by(activityDataImputed, weekFactor, interval)

## Create a dataframe with sum of steps per date
stepsByInterval_weekday <- summarize(filter(activityByInterval_2, weekFactor=="weekday"), meanSteps = mean(steps, na.rm=FALSE))
stepsByInterval_weekend <- summarize(filter(activityByInterval_2, weekFactor=="weekend"), meanSteps = mean(steps, na.rm=FALSE))

stepsByInterval_2 <- summarize(activityByInterval_2, meanSteps = mean(steps, na.rm=FALSE))

# Create the plot
par(mfrow=c(1,1))  # make 2 plots fit
plot(x=stepsByInterval_weekday$interval, y=stepsByInterval_weekday$meanSteps, type="l")
plot(x=stepsByInterval_weekend$interval, y=stepsByInterval_weekend$meanSteps, type="l")

xyplot(stepsByInterval_2$meanSteps ~ stepsByInterval_2$interval | weekFactor, data=stepsByInterval_2, type="l")

