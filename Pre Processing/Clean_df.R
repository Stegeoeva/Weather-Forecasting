setwd('C:\\Users\\user\\Desktop\\IoTFeds\\WeatherData\\Pre Processing')
WorkingEnviroment <- setwd('C:\\Users\\user\\Desktop\\IoTFeds\\WeatherData\\Pre Processing')
library("rjson")
Data <- fromJSON(file="measurements.json")
df <- do.call(rbind, Data)
df <- as.data.frame(df)
#View(df)
#library(openxlsx)
#write.xlsx(df, "dataset.xlsx")
#summary(df)
df <- df[, !(names(df) %in% c("created_at", "updated_at", "id", "node"))]

cols_to_convert <- setdiff(names(df), "dt")
df[cols_to_convert] <- lapply(df[cols_to_convert], function(x) as.numeric(as.character(x)))
#str(df)
df$dt <- as.POSIXct(as.character(df$dt), format = "%Y-%m-%d %H:%M:%S")
library(lubridate)
library(dplyr)
df <- df %>% mutate(dt = with_tz(as.POSIXct(dt, tz = "UTC"), tzone = "Europe/Athens"))

#Create Date and Time columns
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
  periods <- Filter(function(x) length(x) > 3, periods)
  
  return(periods)
}

#Print consecutive records
consecutive_periods <- find_consecutive_periods(unique(df$Date))
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
df3<- dataframes[[3]] # For the range "2023-09-29" to "2023-10-07"
df4 <- dataframes[[4]] # For the range "2023-10-20" to "2023-10-28"
df5 <- dataframes[[5]] # For the range "2023-11-02" to "2023-11-05"

# Reset the row names to sequentially number them from 1
rownames(df2) <- NULL

library(ggplot2)
ggplot(df2, aes(x = dt, y = temperature)) +
  geom_line() +  # Use geom_line for a line plot
  theme_minimal() +
  labs(title = "Temperature Over Time",
       x = "Datetime",
       y = "Temperature")

#Check for gaps
df2$dt <- as.POSIXct(df2$dt)
time_diffs <- diff(df2$dt)
time_diffs_mins <- as.numeric(time_diffs, units = "mins")
threshold <- 35 
# Identify gaps
gaps <- which(time_diffs_mins > threshold)
# Print the gaps
if (length(gaps) > 0) {
  for (i in gaps) {
    cat("Gap between record",   i, "and",   i + 1, ":", time_diffs_mins[i], "minutes\n")
  }
} else {
  cat("No significant gaps found.\n")
}

# Drop the first 3 and last 3 rows
df2 <- df2[-c(1:3, (nrow(df2)-2):nrow(df2)), ]

# Identify the Missing Time Point
missing_time <- as.POSIXct("2023-09-23 17:49:49")
# Create a new row with NA for all columns
missing_row <- df2[1, ]
missing_row[] <- NA  # Set all values to NA
missing_row$dt <- missing_time  # Set the datetime
# Insert the row into the dataframe
df2 <- rbind(df2, missing_row)
df2 <- df2[order(df2$dt),]  # Reorder by date-time
# Perform Linear Interpolation
library(zoo)
# Define columns to exclude from interpolation
exclude_cols <- c("dt", "Date", "Time")
# Apply linear interpolation to each column, excluding the specified ones
df2[, !names(df2) %in% exclude_cols] <- lapply(df2[, !names(df2) %in% exclude_cols], function(x) {
  if(sum(!is.na(x)) >= 2) {  # Check if there are at least two non-NA values
    return(na.approx(x, na.rm = FALSE))
  } else {
    return(x)  # Return the column as-is if not enough non-NA values for interpolation
  }
})
# Reset the row names to sequentially number them from 1
rownames(df2) <- NULL

#Check for nar(rows)
df2$dt <- as.POSIXct(df2$dt)
time_diffs <- diff(df2$dt)
time_diffs_mins <- as.numeric(time_diffs, units = "mins")
threshold <- 25 
# Identify gaps
narrows <- which(time_diffs_mins < threshold)

# Print the nar(rows)
if (length(narrows) > 0) {
  for (i in narrows) {
    cat("Nar(row) between record",  i, "and",  i + 1, ":", time_diffs_mins[i], "minutes\n")
  }
} else {
  cat("No significant nar(rows) found.\n")
}

#Delete nar(rows)
df2 <- df2[df2$dt != '2023-09-26 18:09:25', ]
# Reset the row names to sequentially number them from 1
rownames(df2) <- NULL

# Find duplicated values (
duplicates <- df2[duplicated(df2$column_name), ]
all_duplicates <- df2[df2$column_name %in% df2$column_name[duplicated(df2$column_name)], ]
num_duplicates <- nrow(duplicates)
print(paste("Number of duplicated rows (excluding first occurrence):", num_duplicates))
if(num_duplicates > 0) {
  print("Duplicated rows:")
  print(duplicates)
}

# Check for repeated values. Sensor issues?
for(column_name in names(df2)) {
  # Calculate the counts of each unique value in the column
  value_counts <- table(df2[[column_name]])
  # Find the values that are repeated more than 10 times
  repeated_values <- value_counts[value_counts > 10]
  # Check if there are any repeated values
  if(length(repeated_values) > 0) {
    cat("\nRepeated values in", column_name, ":\n")
    print(repeated_values)
  } else {
    cat("\nNo repeated values found in", column_name, ".\n")
  }
}


#check for outliers visually
boxplot(df2$temperature, main = "Boxplot for Temperature", horizontal = TRUE)
boxplot(df2$humidity, main = "Boxplot for Humidity", horizontal = TRUE)
boxplot(df2$luminosity, main = "Boxplot for Luminosity", horizontal = TRUE)
boxplot(df2$atmospheric_pressure, main = "Boxplot for atm", horizontal = TRUE)
boxplot(df2$co, main = "Boxplot for co", horizontal = TRUE)
boxplot(df2$co2, main = "Boxplot for co2", horizontal = TRUE)
boxplot(df2$pm2_5, main = "Boxplot for pm2_5", horizontal = TRUE)


# Define reasonable ranges for each variable
ranges <- list(
  temperature = c(-10, 40),
  humidity = c (0,100),
  luminosity = c(0, 100000),
  atmospheric_pressure = c(970, 1080),
  co = c(0, 300),
  co2 = c(0, 1500),
  pm2_5 = c(0, 150)
)
# Check for out-of-range values in each column
for (col in names(ranges)) {
  out_of_range <- df2[df2[[col]] < ranges[[col]][1] | df2[[col]] > ranges[[col]][2], ]
  if (nrow(out_of_range) > 0) {
    cat("Rows with out-of-range", col, "values:\n")
    print(out_of_range)
  } else {
    cat("No out-of-range", col, "values found.\n")
  }
}

# Create new columns with default value 0
df2$Day <- 0
df2$Afternoon <- 0

# Update the values based on the Time condition
df2$Day[format(df2$dt, "%H:%M:%S") >= "06:00:00" & format(df2$dt, "%H:%M:%S") < "14:00:00"] <- 1
df2$Afternoon[format(df2$dt, "%H:%M:%S") >= "14:00:00" & format(df2$dt, "%H:%M:%S") < "22:00:00"] <- 1

# Dropping specified columns from the dataframe
dataset <- df2[, !(names(df2) %in% c("battery_voltage", "battery_level", "Date", "Time", "dry", "dt"))]

#library(dplyr)
# Creating new columns with lagged values
#dataset <- dataset %>% mutate(across(everything(), lag, .names = "{.col}_1"))

# View the first few rows of the new dataframe
#head(dataset)

#dataset <- dataset[, !(names(dataset) %in% c("wind_speed","co", "co2","rain_percentage",'rain_percentage_1',"pm2_5", "Afternoon_1",'Day_1'))]
#dataset <- na.omit(dataset)
#rownames(dataset) <- NULL

library(dplyr)

# Creating new columns with t-2 lagged values
dataset <- dataset %>%
  mutate(across(everything(), lag, n = 2, .names = "{.col}_2"))

# View the first few rows of the new dataframe
head(dataset)

# Dropping specific columns and handling missing values
dataset <- dataset[, !(names(dataset) %in% c("wind_speed", "co", "co2", "rain_percentage", "rain_percentage_2", "pm2_5", "Afternoon_2", "Day_2"))]
dataset <- na.omit(dataset)
rownames(dataset) <- NULL

summary(dataset)
#View(dataset)

library(openxlsx)
write.xlsx(dataset, "MyData2.xlsx")
#summary(dataset)
