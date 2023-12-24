# Group Assigment 1

#Libraries
library(corrplot)

# read in the data
data <- read.table("./Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")

# print data in first 5 rows
head(data)

## 1

# create a new column with date and time
data$DateTime <- paste(data$Date, data$Time, sep = " ")
data$DateTime <- as.POSIXlt(data$DateTime, format = "%d/%m/%Y %H:%M:%S")

# get all the data spanning 3rd week of January
dataJan3 <- data[strftime(data$DateTime, format = "%W") == "03",]

# print data in first 5 rows
head(dataJan3)

# compute arithmetic and the geometric mean, the median, the mode and the standard deviation for column index 2, 3 and 4 respectively
# for the data spanning 3rd week of January
arithmeticMean_GAP <- mean(dataJan3$Global_active_power)
arithmeticMean_GRP <- mean(dataJan3$Global_reactive_power)
arithmeticMean_VLT <- mean(dataJan3$Voltage)

geometricMean_GAP <- exp(mean(log(dataJan3$Global_active_power)))
geometricMean_GRP <- exp(mean(log(dataJan3$Global_reactive_power)))
geometricMean_VLT <- exp(mean(log(dataJan3$Voltage)))

median_GAP <- median(dataJan3$Global_active_power)
median_GRP <- median(dataJan3$Global_reactive_power)
median_VLT <- median(dataJan3$Voltage)

mode_GAP <- as.numeric(names(sort(table(dataJan3$Global_active_power), decreasing = TRUE)[1]))
mode_GRP <- as.numeric(names(sort(table(dataJan3$Global_reactive_power), decreasing = TRUE)[1]))
mode_VLT <- as.numeric(names(sort(table(dataJan3$Voltage), decreasing = TRUE)[1]))

standardDeviation_GAP <- sd(dataJan3$Global_active_power)
standardDeviation_GRP <- sd(dataJan3$Global_reactive_power)
standardDeviation_VLT <- sd(dataJan3$Voltage)

# separate the data into weekdays and weekends
dataJan3Weekdays <- dataJan3[weekdays(dataJan3$DateTime) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),] # nolint: line_length_linter.
dataJan3Weekends <- dataJan3[weekdays(dataJan3$DateTime) %in% c("Saturday", "Sunday"),]

# daytime is from 8am to 6pm
# nighttime is from 7pm to 7am
# separate the weekdays data into daytime and nighttime

dataJan3WeekdaysDaytime <- dataJan3Weekdays[as.integer(strftime(dataJan3Weekdays$DateTime, format = "%H")) %in% c(8:18),]
dataJan3WeekdaysNighttime <- dataJan3Weekdays[as.integer(strftime(dataJan3Weekdays$DateTime, format = "%H")) %in% c(19:7),]

# separate the weekends data into daytime and nighttime
dataJan3WeekendsDaytime <- dataJan3Weekends[as.integer(strftime(dataJan3Weekends$DateTime, format = "%H")) %in% c(8:18),]
dataJan3WeekendsNighttime <- dataJan3Weekends[as.integer(strftime(dataJan3Weekends$DateTime, format = "%H")) %in% c(19:7),]

# get min and max values
min_GAP_weekdays_day <- min(dataJan3WeekdaysDaytime$Global_active_power)
max_GAP_weekdays_day <- max(dataJan3WeekdaysDaytime$Global_active_power)

min_GAP_weekdays_night <- min(dataJan3WeekdaysNighttime$Global_active_power)
max_GAP_weekdays_night <- max(dataJan3WeekdaysNighttime$Global_active_power)

min_GAP_weekends_day <- min(dataJan3WeekendsDaytime$Global_active_power)
max_GAP_weekends_day <- max(dataJan3WeekendsDaytime$Global_active_power)

min_GAP_weekends_night <- min(dataJan3WeekendsNighttime$Global_active_power)
max_GAP_weekends_night <- max(dataJan3WeekendsNighttime$Global_active_power)

## 2

print(dataJan3[2, 3:9])

# create correlation matrix for the data spanning 3rd week of January
correlationMatrix <- cor(dataJan3[, 3:9], method = c("spearman"))

# print correlation matrix
print(correlationMatrix)

# plot color-coded correlation matrix
corrplot(correlationMatrix, method = "color")

## 3

print(mean(dataJan3$Global_intensity))