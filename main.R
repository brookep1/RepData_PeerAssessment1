# Need doBy library for summaryBy
library(doBy)
# Read in the RAW data
RAW <- read.csv("../../data/repdata_data_activity/activity.csv", header = TRUE, sep = ",")

# Data manipulations
# Make the date a date object
RAW <- within(RAW, date <- as.Date(date,"%Y-%m-%d"))
# We'll need weekdays later
RAW <- within(RAW, weekday <- weekdays(date))
# Thirty minute time blocks for later
RAW <- within(RAW, block <- trunc(interval/30))

# Summarizations
##### TODO: Move these closer to where they are used
stepsBydate <- summaryBy(steps ~ date, data = RAW, FUN=c(mean,median,sum,sd), na.rm = TRUE)
#
stepsByinterval <- summaryBy(steps ~ interval, data = RAW, FUN=c(mean,median,sum,sd), na.rm = TRUE)
#
stepsByblock <- summaryBy(steps ~ block, data = RAW, FUN=c(mean,median,sum,sd), na.rm = TRUE)
#
stepsByweekday <- summaryBy(steps ~ weekday, data = RAW, FUN=c(mean,median,sum,sd), na.rm = TRUE)
#
stepsBydayblock <- summaryBy(steps ~ block + weekday, data = RAW, FUN=c(mean,median,sum,sd), na.rm = TRUE)
#
stepsBydayint <- summaryBy(steps ~ interval + weekday, data = RAW, FUN=c(mean,median,sum,sd), na.rm = TRUE)

# Take the 5 minute intervals and make larger 15 minute blocks



##### What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in the dataset.
# 
# 1. Calculate the total number of steps taken per day
# .... plot of a per day sum ...
with(stepsBydate,plot(x=date,y=steps.sum))
# TODO: Add a trend line
# ... TOTAL Total ...
sum(stepsBydate$steps.sum)
# 
# 2. Make a histogram of the total number of steps taken each day
with(stepsBydate,hist(steps.sum))
# 
# 3. Calculate and report the mean and median total number of steps taken per day
# ....The mean is ...
with(stepsBydate, mean(steps.sum))
# ....The median is ...
with(stepsBydate, median(steps.sum))

#### What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
#the average number of steps taken, averaged across all days (y-axis)
# .... plot of a per interval average  ...
with(stepsByinterval,plot(x=interval,y=steps.mean,type="l"))

# 2. Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
# .... The interval is .....
with(stepsByinterval,stepsByinterval[steps.mean == max(stepsByinterval$steps.mean),"interval"])

######Imputing missing values

# Note that there are a number of days/intervals where there are missing values (coded as NA). The #presence of missing days may introduce bias into some calculations or summaries of the data.
# 
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# ...the number of "incomplete cases" ....
summary(complete.cases(RAW))[["FALSE"]]
# 
# 2. Devise a strategy for filling in all of the missing values in the dataset. The #strategy does not #need to be sophisticated. For example, you could use the mean
#/median for that day, or the mean for that #5-minute interval, etc.
# Tried 4 strategies using the lowest average standard deviation as the winner
# ...Will use the mean of a 15minute interval to allow for small differences in a "usual" schedule 
#....Assume similar behavior from day to day. We average the 5 minute intervals into 30 minute blocks. This allows for some timing differences from day to day. When guesstimating unavailable values. ...
# ...This is further factored by day to taken into account differences in patterns on different days ...
mean(stepsBydate$steps.sd,na.rm=T)
mean(stepsByweekday$steps.sd,na.rm=T)
mean(stepsByblock$steps.sd,na.rm=T)
mean(stepsBydayblock$steps.sd,na.rm=T)

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# 
# 4. Make a histogram of the total number of steps taken each day and Calculate and 
##4 a.  report the mean and #median total number of steps taken per day. 
##4 b.  Do these values differ from the estimates from the first #part of the assignment? 
##4 c.What is the impact of imputing missing data on the estimates of the total daily #number of steps?
