# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
## read file
csvFile <- "./activity.csv"
df <- read.csv(csvFile, colClasses=c("numeric", "Date", "integer"), na.strings=c("NA"))

## extract time dimension
hhmm <- sprintf("%04d", df$interval)
tm.dim <- unique(hhmm)

## assemble dataset with NAs
df1 <- data.frame(
    dt = df$date
	, tm.ix = sapply(1:length(hhmm), function(i) which(hhmm[i] == tm.dim))
	, steps = df$steps
)
```



## What is mean total number of steps taken per day?

```r
df1.bydt <- aggregate(steps ~ dt, df1, mean)

library(lattice)
histogram(~df1.bydt$steps
    , main="Histogram with NAs"
    , xlab="steps per day" 
)
```

![](PA1_template_files/figure-html/steps per day-1.png) 

```r
## calc mean, median
mean1 <- mean(df1.bydt$steps)
median1 <- median(df1.bydt$steps)
c(mean=mean1, median=median1)
```

```
##     mean   median 
## 37.38260 37.37847
```

## What is the average daily activity pattern?


```r
df1.bytm <- aggregate(steps ~ tm.ix, df1, mean)

x.axis.ix <- seq(1, 288, 36) # tick mark every 3 hours
xyplot(
    (steps * 12) ~ tm.ix     # scale to hourly rate
	, data=df1.bytm
	, type="l"
	, main="Daily Pattern with NAs"
	, xlab="time of day"
	, ylab="steps per hour"
	, scales=list(
		x=list(
			at=x.axis.ix
			, labels=tm.dim[x.axis.ix]
		)
	)
)
```

![](PA1_template_files/figure-html/by interval-1.png) 

```r
## get 5 minute interval with most steps
steps.max <- max(df1.bytm$steps)
ix <- which(df1.bytm$steps == steps.max)
tm.dim[ix]
```

```
## [1] "0835"
```

## Imputing missing values


```r
## get number of missing values
(na.cnt <- sum(is.na(df1$steps)))
```

```
## [1] 2304
```

```r
## assemble enhanced dataset, replace NAs with mean interval values
dfm <- merge(df1, df1.bytm, by=c("tm.ix", "tm.ix"))

df2 <- data.frame(
    dt = dfm$dt
	, tm.ix = dfm$tm.ix
	, steps = ifelse(is.na(dfm$steps.x), dfm$steps.y, dfm$steps.x) # replace NAs here
)

df2.bydt <- aggregate(steps ~ dt, df2, mean)

histogram(~df2.bydt$steps
    , main="Histogram with Imputed Values"
    , xlab="steps per day" 
)
```

![](PA1_template_files/figure-html/impute missing values-1.png) 

```r
## get mean, median
mean2 <- mean(df2.bydt$steps)
median2 <- median(df2.bydt$steps)
c(mean=mean2, median=median2)
```

```
##    mean  median 
## 37.3826 37.3826
```

```r
## calc deltas
mean.delta <- mean2 - mean1
median.delta <- median2 - median1
c(mean.delta=mean.delta, median.delta=median.delta)
```

```
##   mean.delta median.delta 
##  0.000000000  0.004127358
```

The total daily number of steps is now higher because 2304 imputed values were added to the dataset.  The mean is unchanged, by design, because missing values were replaced with mean values.  The median changed because the new imputed values are, in all likelihood, asymmetrically distributed above and below the old median.

## Are there differences in activity patterns between weekdays and weekends?


```r
df3 <- data.frame(
    df2
	, class=ifelse(weekdays(df2$dt) %in% c("Saturday", "Sunday")
		, "Weekend"
		, "Weekday"
	)
)

df3.bytmclass <- aggregate(steps ~ tm.ix + class, df3, mean)

x.axis.ix <- seq(1, 288, 36)     # tick mark every 3 hours
xyplot(
	(steps * 12) ~ tm.ix | class # scale to hourly rate
	, data=df3.bytmclass
	, layout=c(1, 2)
	, type="l"
	, main="Weekend vs Weekday"
	, xlab="time of day"
	, ylab="steps per hour"
	, scales=list(
		x=list(
			at=x.axis.ix
			, labels=tm.dim[x.axis.ix]
		)
	)
)
```

![](PA1_template_files/figure-html/weekends vs weekdays-1.png) 

Our subject appears to sleep-in on weekends.  By contrast, there is a lot of activity earlier on weekday mornings suggesting that our subject goes jogging around 8:30a.
