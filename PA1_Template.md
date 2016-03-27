Course Project 1: Reproducible Research
---------------------------------------

### Loading and Preprocessing the Data

Read in files and transform as will be needed downstream.

    activity<-read.csv("activity.csv")
    activity$date<-as.Date(activity$date)

### What is mean total number of steps taken per day?

Calculate the total number of steps for each day, plot in histogram, and
mean/median.

    dailySums<-tapply(activity$steps,activity$date,sum)
    hist(dailySums,breaks=11, main = "Total Number of Steps Each Day", xlab="Number of Steps in a Day")

![](PA1_Template_files/figure-markdown_strict/daysteps-1.png)<!-- -->

    avg<-as.integer(mean(dailySums,na.rm=TRUE))
    med<-median(dailySums,na.rm=TRUE)

The mean of the total number of steps taken per day is 10766 steps.

The median of the total number of steps taken per day is 10765 steps.

### What is the average daily activity pattern?

Prepare and plot time series of 5-minute intervals averaged across days
and find max average steps interval.

    dailyAvg<-tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
    plot(row.names(dailyAvg),dailyAvg,type="l", ylab = "number of steps per 5 minute interval", xlab = "5 minute interval during the 24 hour day")

![](PA1_Template_files/figure-markdown_strict/intervalavgs-1.png)<!-- -->

    max<-names(which.max(dailyAvg))

The 5-minute interval, on average across all the days in the dataset,
which contains the maximum number of steps is 835.

### Imputing missing values

Calculate the number of missing values in the dataset and prepare new
dataset with NAs replaced with interval means.

    NACount<-sum(is.na(activity$steps))
    imputeNA<-numeric()
    dailyAvg<-aggregate(steps ~ interval, data = activity, FUN = mean)
    for (i in 1:nrow(activity)){
        obs<-activity[i,]
        if (is.na(obs$steps)){
            steps<-subset(dailyAvg, interval == obs$interval)$steps
        } else {
            steps<-obs$steps
        }
        imputeNA<-c(imputeNA,steps)
    }
    activity2.0<-activity
    activity2.0$steps<-imputeNA
    dailySums2.0<-tapply(activity2.0$steps,activity2.0$date,sum)
    hist(dailySums2.0,breaks=11, main = "Total Number of Steps Each Day, Imputed", xlab="Number of Steps in a Day")

![](PA1_Template_files/figure-markdown_strict/impute-1.png)<!-- -->

    avg2.0<-as.integer(mean(dailySums2.0))
    med2.0<-as.integer(median(dailySums2.0))

The calculated total number of missing values in the dataset is 2304.

The mean using imputed means to replace NAs of the total number of steps
taken per day is 10766 steps.

The median using imputed means to replace NAs of the total number of
steps taken per day is 10766 steps.

**There is no difference between the imputed estimates from the first
part of the assignment.**

### Are there differences in activity patterns between weekdays and weekends?

Create new factor variable in imputed dataset by weekend and weekday and
panel plot.

    days<-weekdays(activity$date, abbreviate = FALSE)
    dayLevel<-vector()
    for (i in 1:nrow(activity2.0)){
        if (days[i] == "Saturday") {
            dayLevel[i]<-"weekend"
        } else if (days[i] == "Sunday") {
            dayLevel[i]<-"weekend"
        } else {
            dayLevel[i]<-"weekday"
        }
    }
    activity2.0$dayLevel<-dayLevel
    activity2.0$dayLevel<-factor(activity2.0$dayLevel)
    dayTypeSteps<-aggregate(steps ~ interval + dayLevel, data = activity2.0, mean)
    require(lattice)

    ## Loading required package: lattice

    xyplot(steps ~ interval|dayLevel,dayTypeSteps, type = "l", layout = c(1,2), xlab = "interval", ylab = "Number of Steps by Interval, Average")

![](PA1_Template_files/figure-markdown_strict/unnamed-chunk-1-1.png)<!-- -->

When comparing the weekend to the weekday averages, the subject is less
active until later in the morning, more active throughout the day, and
remains active later into the evening.
