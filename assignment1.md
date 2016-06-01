Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data \[52K\] The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
**date**: The date on which the measurement was taken in YYYY-MM-DD format
**interval**: Identifier for the 5-minute interval in which measurement was taken

Assignment
----------

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2).

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

### Loading and preprocessing the data

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
activity<-read.csv(file="data/activity.csv")
activity$date<-as.Date(activity$date)
```

Here is a sneak peek into the data we will be playing around with :

``` r
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### What is mean total number of steps taken per day?

1.  Calculate the total number of steps taken per day

``` r
agg_steps<-activity %>%
  group_by(date) %>%
  summarise(total = sum(steps,na.rm = TRUE))
head(agg_steps)
```

    ## Source: local data frame [6 x 2]
    ## 
    ##         date total
    ##       (date) (int)
    ## 1 2012-10-01     0
    ## 2 2012-10-02   126
    ## 3 2012-10-03 11352
    ## 4 2012-10-04 12116
    ## 5 2012-10-05 13294
    ## 6 2012-10-06 15420

1.  Histogram of the total number of steps taken each day

``` r
hist(agg_steps$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day")
```

![](assignment1_files/figure-markdown_github/unnamed-chunk-5-1.png)<!-- -->

1.  Calculate and report the mean and median of the total number of steps taken per day

``` r
mean(agg_steps$total)
```

    ## [1] 9354.23

``` r
median(agg_steps$total)
```

    ## [1] 10395

### What is the average daily activity pattern?

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
mean_steps<-activity %>%
  group_by(interval) %>%
  summarise(mean = mean(steps,na.rm = TRUE))
head(mean_steps)
```

    ## Source: local data frame [6 x 2]
    ## 
    ##   interval      mean
    ##      (int)     (dbl)
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

``` r
plot(mean~interval,
     data = mean_steps,
     type="l",
     col="red",
     lwd=2,
     ylab="Mean Steps",
     xlab="Interval",
     main="Time-series of the average number of steps per interval")
```

![](assignment1_files/figure-markdown_github/unnamed-chunk-8-1.png)<!-- -->

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
mean_steps$interval[mean_steps$mean == max(mean_steps$mean,na.rm = TRUE)]
```

    ## [1] 835

### Inputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(activity$steps))
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    **Strategy: ** Using mean for that interval to substitute the NA values
    **Approach: ** Performed an outer join of the activity and mean\_steps tables created earlier based on interval and then substituted the steps where there are NA values

``` r
names(mean_steps)<-c("interval","steps")
temp<-merge(activity,mean_steps,by = "interval",all=TRUE)
head(temp)
```

    ##   interval steps.x       date  steps.y
    ## 1        0      NA 2012-10-01 1.716981
    ## 2        0       0 2012-11-23 1.716981
    ## 3        0       0 2012-10-28 1.716981
    ## 4        0       0 2012-11-06 1.716981
    ## 5        0       0 2012-11-24 1.716981
    ## 6        0       0 2012-11-15 1.716981

``` r
temp<-temp[order(as.Date(temp$date, format="%Y/%m/%d")),]
for(i in 1:nrow(temp)){
  if(is.na(temp[[2]][i]))
    temp[[2]][i]<-temp[[4]][i]
}
```

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

Assigning the vector created in previous step to activity$steps

``` r
activity$steps<-temp$steps.x
head(activity)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

1.  Make a histogram of the total number of steps taken each day

``` r
agg_steps_new<-activity %>%
  group_by(date) %>%
  summarise(total = sum(steps))
hist(agg_steps_new$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day(New Dataset)")
```

![](assignment1_files/figure-markdown_github/unnamed-chunk-13-1.png)<!-- -->

1.  Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
mean(agg_steps_new$total)
```

    ## [1] 10766.19

``` r
median(agg_steps_new$total)
```

    ## [1] 10766.19

We see here that the mean has increased from 9354.23 to 10766.19. This increase can be attributed to the substitution of missing values with perhaps-slightly higher values.
We can also see that the resultant mean and median are almost same. This indicates that the resultant data is evenly divided around the mean since the value that minimizes the sum of absolute deviations(i.e the median) is the same as the mean.

### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
activity$day<-weekdays(activity$date)
activity <- cbind(activity, dayType=ifelse(activity$day == "Saturday" |
                                           activity$day == "Sunday",
                                           "weekend",
                                           "weekday"))
head(activity)
```

    ##       steps       date interval    day dayType
    ## 1 1.7169811 2012-10-01        0 Monday weekday
    ## 2 0.3396226 2012-10-01        5 Monday weekday
    ## 3 0.1320755 2012-10-01       10 Monday weekday
    ## 4 0.1509434 2012-10-01       15 Monday weekday
    ## 5 0.0754717 2012-10-01       20 Monday weekday
    ## 6 2.0943396 2012-10-01       25 Monday weekday

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
mean_steps<-activity %>%
  group_by(dayType,day,interval) %>%
  summarise(mean = mean(steps,na.rm = TRUE))
library(lattice)
xyplot(mean ~ interval | dayType, mean_steps, 
       type="l", 
       lwd=1.5, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```

![](assignment1_files/figure-markdown_github/unnamed-chunk-16-1.png)<!-- -->

We can see here that the plot for weekdays is naturally denser due to more number of points on weekdays.We also see that the higher values in the range of 150-300 are attained more often on weekdays as opposed to weekends.
