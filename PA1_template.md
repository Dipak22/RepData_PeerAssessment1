
### Load data from the file x.csv and preprocess it.

```r
unzip("repdata_data_activity.zip")
x <- read.csv("activity.csv")
```


### (Solution) What is the mean total number of steps taken per day?

1. Below code makes a histogram of the total number of steps taken each day


```r
steps_each_day <- aggregate(steps ~ date, data = x, FUN = sum)
barplot(steps_each_day$steps, names.arg = steps_each_day$date, xlab = "date -->", 
    ylab = "number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


2. Below Code calculate and report the *mean* and *median* total number of
   steps taken per day


```r
mean(steps_each_day$steps)
```

```
## [1] 10766
```

```r
median(steps_each_day$steps)
```

```
## [1] 10765
```


### (Solution) What is the average daily activity pattern?

1. Below code makes a time series plot (i.e. `type = "l"`) of the 5-minute
   interval (x-axis) and the average number of steps taken, averaged
   across all days (y-axis)


```r
steps_per_interval <- aggregate(steps ~ interval, data = x, FUN = mean)
plot(steps_per_interval, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


2. Below code helps to find which 5-minute interval, on average across all the days in the
   dataset, contains the maximum number of steps?


```r
steps_per_interval$interval[which.max(steps_per_interval$steps)]
```

```
## [1] 835
```



### (Solution) Imputing missing values

1. Below code calculates and report the total number of missing values in the
   dataset (i.e. the total number of rows with `NA`s)


```r
sum(is.na(x))
```

```
## [1] 2304
```


2. ** Question) ** Devise a strategy for filling in all of the missing values in the
   dataset. The strategy does not need to be sophisticated. For
   example, you could use the mean/median for that day, or the mean
   for that 5-minute interval, etc.


    Here, means for the 5-minute intervals are used as fillers for the missing values.
    

3. Below code creates a new dataset that is equal to the original dataset but with
   the missing data filled in.


```r
x <- merge(x, steps_per_interval, by = "interval", suffixes = c("", "_mn"))
nas <- is.na(x$steps)
x$steps[nas] <- x$steps_mn[nas]
x <- x[, c(1:3)]
```


4. Below code makes a histogram of the total number of steps taken each day and
   Calculate and report the **mean** and **median** total number of
   steps taken per day. 


```r
steps_each_day <- aggregate(steps ~ date, data = x, FUN = sum)
barplot(steps_each_day$steps, names.arg = steps_each_day$date, xlab = "date", 
    ylab = "steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(steps_each_day$steps)
```

```
## [1] 10766
```

```r
median(steps_each_day$steps)
```

```
## [1] 10766
```


  **Question)**Do these values differ from the estimates from the first part of the assignment?
  What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
  When estimating the total number of steps/day, the impact of imputing missing values with the mean is 
  negligible.


### (Solution) Are there differences in x patterns between weekdays and weekends?

1. Below Code creates a new factor variable in the dataset with two levels --
   "weekday" and "weekend" indicating whether a given date is a
   weekday or weekend day.


```r
data_type <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
x$data_type <- as.factor(sapply(x$date, data_type))
```


2. Below Code makes a panel plot containing a time series plot (i.e. `type = "l"`)
   of the 5-minute interval (x-axis) and the average number of steps
   taken, averaged across all weekday days or weekend days
   (y-axis).


```r
library(lattice)

steps.type <- aggregate(steps ~ interval + data_type, data = x, FUN = mean)

xyplot(steps ~ interval | data_type, data = steps.type, layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

