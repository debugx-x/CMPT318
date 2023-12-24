# Group Assignment 3

#Libraries
library(depmixS4)
library(dplyr)


# read in the data
data <- read.table("./Group_Assignment_3_Dataset.txt", header = TRUE, sep = ",")

# create a new column with date and time
data$DateTime <- paste(data$Date, data$Time, sep = " ")
data$DateTime <- as.POSIXlt(data$DateTime, format = "%d/%m/%Y %H:%M:%S")

# print data in first 5 rows
head(data)

## Q1

# scale the data and keep only the column Global_intensity
data_scaled <- scale(data[, "Global_intensity"])

# merge of data and scaled data
data_scaled <- cbind(data_scaled, data[,c("Date", "Time", "DateTime")])

# rename the column
names(data_scaled)[1] <- "Global_intensity"

# print scaled data in first 5 rows
head(data_scaled)

# filter the data for all weekdays
data_weekdays <- data_scaled[weekdays(data_scaled$DateTime) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), ]

# filter time window between 7:00 and 20:00 based on 22/02/2007
data_train <- data_weekdays[data_weekdays$Time >= "06:00:00" & data_weekdays$Time <= "20:00:00",]


# loop over the weekdays data based on date
# for (date in unique(data_weekdays$Date)) {
#   # filter the data for the date
#   data_date <- data_weekdays[data_weekdays$Date == date, ]

#   # create a range of time windows for the day
#   time_windows <- seq(min(data_date$DateTime), max(data_date$DateTime), by = "2 hours")

#   # loop over the time windows
#   for (time_window in time_windows) {
#     # filter the data for the time window
#     data_date <- data_date[data_date$DateTime < time_window, ]

#     # print the mean of the data
#     print(mean(data_date$Global_intensity))
#   }

#   # filter the data for the time window
#   data_date <- data_date[data_date$DateTime < time_window, ]

#   # print the mean of the data
#   print(mean(data_date$Global_intensity))
#   print(nrow(data_date))
# }

## Q2

# set seed
set.seed(301386847)

# create HMM model using depmixS4 package nstate = 5
model_1 <- depmix(response = Global_intensity ~ 1, data = data_train, nstates = 5 )
fitModel_1 <- fit(model_1)
summary(fitModel_1)
print(fitModel_1)

# create HMM model using depmixS4 package nstate = 10
model_2 <- depmix(response = Global_intensity ~ 1, data = data_train, nstates = 10 )
fitModel_2 <- fit(model_2)
summary(fitModel_2)
print(fitModel_2)

# create HMM model using depmixS4 package nstate = 15
model_3 <- depmix(response = Global_intensity ~ 1, data = data_train, nstates = 15 )
fitModel_3 <- fit(model_3)
summary(fitModel_3)
print(fitModel_3)
