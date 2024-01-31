setwd('C:\\Users\\user\\Desktop\\IoTFeds\\WeatherData\\Pre Processing')
WorkingEnviroment <- setwd('C:\\Users\\user\\Desktop\\IoTFeds\\WeatherData\\Pre Processing')
library("rjson")
Data <- fromJSON(file="measurements.json")
df <- do.call(rbind, Data)
df <- as.data.frame(df)
View(df)
#library(openxlsx)
#write.xlsx(df, "dataset.xlsx")
#summary(df)
df <- df[, !(names(df) %in% c("created_at", "updated_at", "id", "node"))]

cols_to_convert <- setdiff(names(df), "dt")
df[cols_to_convert] <- lapply(df[cols_to_convert], function(x) as.numeric(as.character(x)))
str(df)

df$dt <- as.POSIXct(as.character(df$dt), format = "%Y-%m-%d %H:%M:%S")
summary(df)

df$Date <- as.Date(df$dt)
df$Time <- format(df$dt, "%H:%M:%S")

# Calculate the number of unique dates
num_unique_dates <- length(unique(df$Date))
num_unique_dates

find_consecutive_periods <- function(dates) {
  # Ensure the dates are sorted and in Date format
  dates <- sort(as.Date(dates))
  
  # Find the differences between consecutive dates
  date_diffs <- c(1, diff(dates))
  
  # Identify the starts and ends of consecutive periods
  starts <- which(date_diffs != 1)
  ends <- c(starts - 1, length(dates))
  starts <- c(1, starts)
  
  # Extract the periods
  periods <- Map(function(start, end) dates[start:end], starts, ends)
  
  # Filter out periods that are not consecutive (length = 1)
  periods <- Filter(function(x) length(x) > 1, periods)
  
  return(periods)
}

# Example usage
consecutive_periods <- find_consecutive_periods(df$Date)
consecutive_periods

# Define date ranges
date_ranges <- list(
  c("2023-04-25", "2023-05-02"),
  c("2023-09-15", "2023-09-27"),
  c("2023-09-29", "2023-10-07"),
  c("2023-10-20", "2023-10-28"),
  c("2023-11-02", "2023-11-05")
)

# Function to create a subset dataframe for a given date range
create_subset_df <- function(start_date, end_date, df) {
  subset(df, Date >= as.Date(start_date) & Date <= as.Date(end_date))
}

# Create separate dataframes for each date range
dataframes <- lapply(date_ranges, function(range) create_subset_df(range[1], range[2], df))

# Access the dataframes
df1 <- dataframes[[1]] # For the range "2023-04-25" to "2023-05-02"
df2 <- dataframes[[2]] # For the range "2023-09-15" to "2023-09-27"
df3 <- dataframes[[3]] # For the range "2023-09-29" to "2023-10-07"
df4 <- dataframes[[4]] # For the range "2023-10-20" to "2023-10-28"
df5 <- dataframes[[5]] # For the range "2023-11-02" to "2023-11-05"

library(ggplot2)
ggplot(df2, aes(x = dt, y = temperature)) +
  geom_line() +  # Use geom_line for a line plot
  theme_minimal() +
  labs(title = "Temperature Over Time",
       x = "Datetime",
       y = "Temperature")

df2$dt <- as.POSIXct(df2$dt)
time_diffs <- diff(df2$dt)
time_diffs_mins <- as.numeric(time_diffs, units = "mins")
threshold <- 45 
# Identify gaps
gaps <- which(time_diffs_mins > threshold)
# Print the gaps
if (length(gaps) > 0) {
  for (i in gaps) {
    cat("Gap between record", 376 + i, "and", 376 + i + 1, ":", time_diffs_mins[i], "minutes\n")
  }
} else {
  cat("No significant gaps found.\n")
}

# Drop the first 3 and last 3 rows
df2 <- df2[-c(1:3, (nrow(df2)-2):nrow(df2)), ]

# Create new columns with default value 0
df2$Day <- 0
df2$Afternoon <- 0

# Update the values based on the Time condition
df2$Day[format(df2$dt, "%H:%M:%S") >= "04:00:00" & format(df2$dt, "%H:%M:%S") < "12:00:00"] <- 1
df2$Afternoon[format(df2$dt, "%H:%M:%S") >= "12:00:00" & format(df2$dt, "%H:%M:%S") < "20:00:00"] <- 1

# Dropping specified columns from the dataframe
dataset <- df2[, !(names(df2) %in% c("battery_voltage", "battery_level", "Date", "Time", "dry", "dt"))]

library(dplyr)
# Creating new columns with lagged values
dataset <- dataset %>% mutate(across(everything(), lag, .names = "{.col}_1"))

# View the first few rows of the new dataframe
head(dataset)

dataset <- dataset[, !(names(dataset) %in% c("wind_speed","co", "co2","rain_percentage","pm2_5", "Afternoon_1",'Day_1'))]
dataset <- na.omit(dataset)

library(openxlsx)
write.xlsx(dataset, "MyData.xlsx")
summary(dataset)

