setwd("/Users/okada/myWork/coursera/RepData_PeerAssessment1")
library(plyr);library(ggplot2)
## load data and transform the data
dat <- read.csv("activity.csv")
## make a datetime column
dat$datetime <- as.POSIXct(strptime(paste(dat$date, formatC(dat$interval, width=4, flag="0")), 
                                    "%Y-%m-%d %H%M"), "GMT")
dim(dat);head(dat)

unique(dat$date)
dat.sum <- ddply(dat, .(date), summarize, total_steps=(sum(steps, na.rm=TRUE)))
dim(dat.sum);head(dat.sum)
hist(dat.sum$total_steps, ylab="Frequecy", xlab="the total number of steps per day",
     main="Histogram of the total number of steps taken each day")
## mean and median
mean(dat.sum$total_steps);median(dat.sum$total_steps)
# mean : 9354.23  median=10395

# What is the average daily activity pattern?
plot(dat$steps, type="l")
abline(h=mean(dat$steps, na.rm=TRUE), col=2)
plot(dat.interval, type="l")
dat.interval <- ddply(dat, .(interval), summarize, mean_steps = mean(steps, na.rm=TRUE), 
                      med_steps = median(steps, na.rm=TRUE))

# 5-min interval plot
plot(dat.interval$mean_steps, type="l")
abline(h=mean(dat.interval$mean_steps, na.rm=TRUE), col=2)
dat.interval[which.max(dat.interval$mean_steps),]


# inputing missing values
# caluclate the meadian steps rounded to the nearest integer, and replace NA with the median
# of the correspoinding interval
sum(is.na(dat$steps)) # 2304 NA
dat1 <- merge(dat, dat.interval, by="interval")
head(dat1)
dat1$steps.imp <- ifelse(is.na(dat1$steps), dat1$med_steps, dat1$steps)
dat1 <- subset(arrange(dat1, date, interval), select=c(steps, steps.imp, date, datetime, interval))
dat1.sum <- ddply(dat1, .(date), summarize, total_steps=sum(steps.imp))
hist(dat1.sum$total_steps)
mean(dat1.sum$total_steps)
median(dat1.sum$total_steps)
# mean=9504, median=10395  ( mean sligthly changed and increased)

## Are there differences in activity patterns between weekdays and weekends?
Sys.setlocale(category="LC_TIME", "en_US.UTF-8")
dat1$weekend <- ifelse(weekdays(as.POSIXlt(dat1$date)) %in% c("Saturday","Sunday"), "weekend","weekday")
dat1.sum.weekend <- ddply(dat1, .(weekend, interval), summarize, mean_steps=mean(steps.imp, na.rm=TRUE))
library(lattice)
plot(mean_steps~interval|factor(weekend), type="l", data=dat1.sum.weekend)
m <- ggplot(dat1.sum.weekend, aes(x=interval, y= mean_steps))
m <- m + geom_line() + facet_wrap(~weekend, ncol=1) 
m + xlab("Interval") + ylab("Average Number of Steps")

library(knitr)
knit2html(input = "PA1_template.Rmd", output = "PA1_template.Rmd") 
knit2html(input = "PA1_template.Rmd", output = "PA1_template.md") 
