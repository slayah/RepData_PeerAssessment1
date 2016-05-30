library(dplyr)

activity<-read.csv(file="data/activity.csv")
activity$date<-as.Date(activity$date)

#Calculate the total number of steps taken per day
agg_steps<-activity %>%
  group_by(date) %>%
  summarise(total = sum(steps,na.rm = TRUE))

#Histogram of the total number of steps taken each day
hist(agg_steps$total, 
       breaks=seq(from=0, to=25000, by=2500),
       col="green", 
       xlab="Total number of steps", 
       ylim=c(0, 20), 
       main="Histogram of the total number of steps taken each day")

#Mean and median total number of steps taken each day
mean(agg_steps$total)
median(agg_steps$total)

#time series plot of the 5-minute interval and the average number of steps taken, averaged across all days 
mean_steps<-activity %>%
  group_by(interval) %>%
  summarise(mean = mean(steps,na.rm = TRUE))
plot(mean~interval,
     data = mean_steps,
     type="l",
     col="red",
     lwd=2,
     ylab="Mean Steps",
     xlab="Interval",
     main="Time-series of the average number of steps per interval")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
mean_steps$interval[mean_steps$mean == max(mean_steps$mean,na.rm = TRUE)]

#Calculate and report the total number of missing values in the dataset
sum(is.na(activity$steps))

#Devise a strategy for filling in all of the missing values in the dataset
#Using mean for that 5-minute interval
names(mean_steps)<-c("interval","steps")
temp<-merge(activity,mean_steps,by = "interval",all=TRUE)
temp<-temp[order(as.Date(temp$date, format="%Y/%m/%d")),]
for(i in 1:nrow(temp)){
  if(is.na(temp[[2]][i])){
    temp[[2]][i]<-temp[[4]][i]
    k<-k+1
  }
}
activity$steps<-temp$steps.x

#Make a histogram of the total number of steps taken each day
agg_steps_new<-activity %>%
  group_by(date) %>%
  summarise(total = sum(steps))
hist(agg_steps_new$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day(New Dataset)")

#Calculate and report the mean and median total number of steps taken per day
mean(agg_steps_new$total)
median(agg_steps_new$total)

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
activity$day<-weekdays(activity$date)
activity <- cbind(activity, dayType=ifelse(activity$day == "Saturday" |
                                           activity$day == "Sunday",
                                           "weekend",
                                           "weekday"))

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



#head(activity)
#head(agg_steps)
#head(mean_steps)
#head(data[!(is.na(data$steps)),])
#head(temp)
#nrow(activity)
#nrow(temp)