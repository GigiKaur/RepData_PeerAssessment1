library(ggplot2)
library(dplyr)

if (!file.exists("./data")) {
  dir.create("./data")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/activity.zip", method = "curl")

unzip(zipfile = "./data/activity.zip", exdir = "./data")
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date)

# Analysis
stepsPerDay <- activity %>%
  group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE))

hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps",
     col = "lightgreen", xlab = "Steps", ylim = c(0, 30))

meanPreNA <- round(mean(stepsPerDay$sumsteps))
medianPreNA <- round(median(stepsPerDay$sumsteps))

print(paste("The mean is: ", meanPreNA))
print(paste("The median is: ", medianPreNA))

# Section 2
stepsPerInterval <- activity %>%
  group_by(interval) %>%
  summarize(meansteps = mean(steps, na.rm = TRUE))

plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col = "blue", type = "l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")

print(paste("5-Minute Interval containing the most steps on average: ", stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))
print(paste("Average steps for that interval: ", round(max(stepsPerInterval$meansteps))))
print(paste("The total number of rows with NA is: ", sum(is.na(activity$steps))))

# Replace NA values with interval mean
activityNoNA <- activity
for (i in 1:nrow(activity)) {
  if (is.na(activity$steps[i])) {
    activityNoNA$steps[i] <- stepsPerInterval$meansteps[activityNoNA$interval[i] == stepsPerInterval$interval]
  }
}

stepsPerDay <- activityNoNA %>%
  group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE))

hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps",
     col = "lightblue", xlab = "Steps")

meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanPostNA)))
print(paste("The median is: ", median(medianPostNA)))

NACompare <- data.frame(mean = c(meanPreNA, meanPostNA), median = c(medianPreNA, medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)

# Section 4
activityDoW <- activityNoNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)

activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)

g <- ggplot(wkdayWkend, aes(interval, steps))
g + geom_line() + facet_grid(day ~ .) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        panel.background = element_rect(fill = "lightyellow"),
        strip.background = element_rect(fill = "lightblue"),
        strip.text = element_text(color = "darkblue")) +
  labs(y = "Number of Steps") + labs(x = "Interval") +
  ggtitle("Average Number of Steps: Weekday vs. Weekend") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkgreen"))
