setwd('C:\\Users\\user\\Desktop\\IoTFeds\\everweather_Imprv\\everW')
WorkingEnviroment <- setwd('C:\\Users\\user\\Desktop\\IoTFeds\\everweather_Imprv\\everW')
library(readxl)

Data <- read_excel("MyData.xlsx")
summary(Data)
library(corrplot)

# Extract numeric columns
numeric_columns <- sapply(Data, is.numeric)
numeric_data <- Data[, numeric_columns]
# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)
# Visualize the correlation matrix using corrplot
corrplot(cor_matrix, method = "circle", type = 'lower', tl.col = "black", tl.srt = 45)

library(dplyr)
library(tidyr)
library(pls)

Data <- Data[, !names(Data) %in% c('rain_percentage_1')]
# Splitting the dataframe
train_set <- Data[1:430, ]
test <- Data[(431:nrow(Data)), ]
data <- train_set

data1 <- data[, !names(data) %in% c('humidity', 'luminosity', 'co', 'c02', 'pm2_5', 'atmospheric_pressure')]
data2 <- data[, !names(data) %in% c('temperature', 'luminosity', 'co', 'c02', 'pm2_5', 'atmospheric_pressure')]
data3 <- data[, !names(data) %in% c('humidity', 'temperature', 'co', 'c02', 'pm2_5', 'atmospheric_pressure')]
data4 <- data[, !names(data) %in% c('humidity', 'luminosity', 'co', 'c02', 'pm2_5', 'temperature')]

test1 <- test[, !names(test) %in% c('humidity', 'luminosity', 'co', 'co2', 'pm2_5', 'atmospheric_pressure')]
test2 <- test[, !names(test) %in% c('temperature', 'luminosity', 'co', 'co2', 'pm2_5', 'atmospheric_pressure')]
test3 <- test[, !names(test) %in% c('humidity', 'temperature', 'co', 'co2', 'pm2_5', 'atmospheric_pressure')]
test4 <- test[, !names(test) %in% c('humidity', 'luminosity', 'co', 'co2', 'pm2_5', 'temperature')]

#Temperature

#PCR
set.seed(2)
pcr_fit2 = pcr(temperature~., data = data1, scale = TRUE, validation = "CV",segments=5)
validationplot(pcr_fit2, val.type = "RMSEP")
validationplot(pcr_fit2, val.type = 'R2')
#validationplot(pcr_fit2, val.type = 'MSEP')
summary(pcr_fit2)
coefplot(pcr_fit2)
predplot(pcr_fit2)

pcr_rmse <- RMSEP(pcr_fit2)$val
min(pcr_rmse)
# Data for Scree Plot
components <- 1:10
variance_explained <- c(31.86 ,   54.31   , 67.85 ,   77.42  ,  85.12  ,  90.35  ,  94.72  ,  97.09 ,   98.96  ,  100.00)

# Create a data frame for ggplot
scree_data <- data.frame(Components = components, Variance = variance_explained)

library(ggplot2)
# Generate the Scree Plot
ggplot(scree_data, aes(x = Components, y = Variance)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("Scree Plot of PCR Model") + 
  xlab("Number of Components") + 
  ylab("Percentage of Variance Explained")

#PLS

set.seed(1)
pls_fit = plsr(temperature~., data = data1, scale = TRUE, validation = "CV",segments=5 )
summary(pls_fit)

validationplot(pls_fit, val.type = "RMSEP")
validationplot(pls_fit, val.type = 'R2')
#validationplot(pls_fit, val.type = 'MSEP')
coefplot(pls_fit)
predplot(pls_fit)
pls_rmse <- RMSEP(pls_fit)$val
min(pls_rmse)


summary_output <- capture.output(summary(pls_fit))
# Adjust the regular expression based on the exact format of your summary output
variance_pattern <- "X\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
# Extract the variance explained values
variance_matches <- regmatches(summary_output, regexpr(variance_pattern, summary_output))
# Process the extracted string to get numeric values
variance_values <- as.numeric(unlist(strsplit(variance_matches, "\\s+"))[-1])

components <- 1:10
# Plotting the scree plot
plot(components, variance_values, type = "b", xlab = "Number of Components", ylab = "Explained Variance (%)",
     pch = 19, col = "blue", main = "Scree Plot")

#Luminosity

#PCR

pcr_fit2 = pcr(luminosity~., data = data3, scale = TRUE, validation = "CV", segments=5)
validationplot(pcr_fit2, val.type = "RMSEP")
validationplot(pcr_fit2, val.type = 'R2')
#validationplot(pcr_fit2, val.type = 'MSEP')
summary(pcr_fit2)
coefplot(pcr_fit2)
predplot(pcr_fit2)
pcr_rmse <- RMSEP(pcr_fit2)$val
min(pcr_rmse)
summary_output <- capture.output(summary(pcr_fit2))
# Adjust the regular expression based on the exact format of your summary output
variance_pattern <- "X\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
# Extract the variance explained values
variance_matches <- regmatches(summary_output, regexpr(variance_pattern, summary_output))
# Process the extracted string to get numeric values
variance_values <- as.numeric(unlist(strsplit(variance_matches, "\\s+"))[-1])
components <- 1:10
# Plotting the scree plot
plot(components, variance_values, type = "b", xlab = "Number of Components", ylab = "Explained Variance (%)",
     pch = 19, col = "blue", main = "Scree Plot")

#PLS

set.seed(1)
pls_fit = plsr(luminosity~., data = data3, scale = TRUE, validation = "CV",segments=5)
summary(pls_fit)

validationplot(pls_fit, val.type = "RMSEP")
validationplot(pls_fit, val.type = 'R2')
#validationplot(pls_fit, val.type = 'MSEP')
coefplot(pls_fit)
predplot(pls_fit)
pls_rmse <- RMSEP(pls_fit)$val
min(pls_rmse)

summary_output <- capture.output(summary(pls_fit))
# Adjust the regular expression based on the exact format of your summary output
variance_pattern <- "X\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
# Extract the variance explained values
variance_matches <- regmatches(summary_output, regexpr(variance_pattern, summary_output))
# Process the extracted string to get numeric values
variance_values <- as.numeric(unlist(strsplit(variance_matches, "\\s+"))[-1])

components <- 1:10
# Plotting the scree plot
plot(components, variance_values, type = "b", xlab = "Number of Components", ylab = "Explained Variance (%)",
     pch = 19, col = "blue", main = "Scree Plot")

#Humidity
#PCR
pcr_fit2 = pcr(humidity~., data = data2, scale = TRUE, validation = "CV", segments=5)
validationplot(pcr_fit2, val.type = "RMSEP")
validationplot(pcr_fit2, val.type = 'R2')
#validationplot(pcr_fit2, val.type = 'MSEP')
summary(pcr_fit2)
coefplot(pcr_fit2)
predplot(pcr_fit2)
pcr_rmse <- RMSEP(pcr_fit2)$val
min(pcr_rmse)
summary_output <- capture.output(summary(pcr_fit2))
# Adjust the regular expression based on the exact format of your summary output
variance_pattern <- "X\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
# Extract the variance explained values
variance_matches <- regmatches(summary_output, regexpr(variance_pattern, summary_output))
# Process the extracted string to get numeric values
variance_values <- as.numeric(unlist(strsplit(variance_matches, "\\s+"))[-1])
components <- 1:10
# Plotting the scree plot
plot(components, variance_values, type = "b", xlab = "Number of Components", ylab = "Explained Variance (%)",
     pch = 19, col = "blue", main = "Scree Plot")

#PLS

set.seed(1)
pls_fit = plsr(humidity~., data = data2, scale = TRUE, validation = "CV", segments=5)
summary(pls_fit)

validationplot(pls_fit, val.type = "RMSEP")
validationplot(pls_fit, val.type = 'R2')
#validationplot(pls_fit, val.type = 'MSEP')
coefplot(pls_fit)
predplot(pls_fit)
pls_rmse <- RMSEP(pls_fit)$val
min(pls_rmse)
summary_output <- capture.output(summary(pls_fit))
# Adjust the regular expression based on the exact format of your summary output
variance_pattern <- "X\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)\\s+(\\d+\\.\\d+)"
# Extract the variance explained values
variance_matches <- regmatches(summary_output, regexpr(variance_pattern, summary_output))
# Process the extracted string to get numeric values
variance_values <- as.numeric(unlist(strsplit(variance_matches, "\\s+"))[-1])

components <- 1:10
# Plotting the scree plot
plot(components, variance_values, type = "b", xlab = "Number of Components", ylab = "Explained Variance (%)",
     pch = 19, col = "blue", main = "Scree Plot")

#Atmospheric_pressure

#PCR
pcr_fit2 = pcr(atmospheric_pressure~., data = data4, scale = TRUE, validation = "CV", segments=5)
validationplot(pcr_fit2, val.type = "RMSEP")
validationplot(pcr_fit2, val.type = 'R2')
#validationplot(pcr_fit2, val.type = 'MSEP')
summary(pcr_fit2)
coefplot(pcr_fit2)
predplot(pcr_fit2)
pcr_rmse <- RMSEP(pcr_fit2)$val
min(pcr_rmse)

#print("Minimum RMSE for PCR:", min(pcr_rmse))
variance_explained <- c(31.86    ,54.31   , 67.85   , 77.42  ,  85.12   , 90.35  ,  94.72 ,   97.09 ,   98.96, 100.00)

# Create a data frame for ggplot
scree_data <- data.frame(Components = components, Variance = variance_explained)

# Generate the Scree Plot
ggplot(scree_data, aes(x = Components, y = Variance)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("Scree Plot of PCR Model") + 
  xlab("Number of Components") + 
  ylab("Percentage of Variance Explained")

#PLS

set.seed(1)
pls_fit = plsr(atmospheric_pressure~., data = data4, scale = TRUE, validation = "CV", segments=5)
summary(pls_fit)

validationplot(pls_fit, val.type = "RMSEP")
validationplot(pls_fit, val.type = 'R2')
#validationplot(pls_fit, val.type = 'MSEP')
coefplot(pls_fit)
predplot(pls_fit)
pls_rmse <- RMSEP(pls_fit)$val
#print("Minimum RMSE for PLSR:", min(pls_rmse))
min(pls_rmse)

variance_explained <- c(29.25  ,  48.54  ,  59.91 ,   70.71,    77.97,    83.45,    91.42  ,  96.69 ,   98.78,  100.00)

# Create a data frame for ggplot
scree_data <- data.frame(Components = components, Variance = variance_explained)

# Generate the Scree Plot
ggplot(scree_data, aes(x = Components, y = Variance)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("Scree Plot of PLS Model") + 
  xlab("Number of Components") + 
  ylab("Percentage of Variance Explained")

#Test set

pcr_model <- pcr(temperature~., data = data1,scale =TRUE, validation = "CV", segments=5)
pcr_pred <- predict(pcr_model, test1, ncomp =8)
y_test <- test1[, 1]
pcr_pred_numeric <- unlist(pcr_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pcr_pred_numeric) - as.numeric(y_test_numeric))^2))


combined_data1 <- rbind(data1, test1)
# Assuming combined_data is your combined dataframe and pcr_pred is your PCR predictions

# Number of predictions
num_predictions <- length(pcr_pred)

# Initialize the temp_pred column with NAs
combined_data1$temp_pred <- NA

# Replace the last num_predictions values in temp_pred with your PCR predictions
combined_data1[(nrow(combined_data1) - num_predictions + 1):nrow(combined_data1), "temp_pred"] <- pcr_pred

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
plot_data <- data.frame(Index = 1:nrow(combined_data1), 
                        Temperature = combined_data1$temperature, 
                        Temp_Pred = combined_data1$temp_pred)

# Generate the plot
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Temperature, colour = "Actual Temperature")) +
  geom_line(aes(y = Temp_Pred, colour = "Predicted Temperature")) +
  theme_minimal() +
  labs(title = "Temperature vs Predicted Temperature", 
       x = "Index", 
       y = "Temperature") +
  scale_colour_manual("", 
                      breaks = c("Actual Temperature", "Predicted Temperature"),
                      values = c("blue", "red"))




pcr_model <- pcr(humidity~., data = data2,scale =TRUE, validation = "CV", segments=5)
pcr_pred <- predict(pcr_model, test2, ncomp = 7)
y_test <- test2[, 1]
pcr_pred_numeric <- unlist(pcr_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pcr_pred_numeric) - as.numeric(y_test_numeric))^2))

combined_data2 <- rbind(data2, test2)
# Assuming combined_data is your combined dataframe and pcr_pred is your PCR predictions

# Number of predictions
num_predictions <- length(pcr_pred)

# Initialize the temp_pred column with NAs
combined_data2$hum_pred <- NA

# Replace the last num_predictions values in temp_pred with your PCR predictions
combined_data2[(nrow(combined_data2) - num_predictions + 1):nrow(combined_data2), "hum_pred"] <- pcr_pred

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
plot_data <- data.frame(Index = 1:nrow(combined_data2), 
                        Humidity = combined_data2$humidity, 
                        Hum_Pred = combined_data2$hum_pred)

# Generate the plot
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Humidity, colour = "Actual Humidity")) +
  geom_line(aes(y = Hum_Pred, colour = "Predicted Humidity")) +
  theme_minimal() +
  labs(title = "Humidity vs Predicted Humidity", 
       x = "Index", 
       y = "Humidity") +
  scale_colour_manual("", 
                      breaks = c("Actual Humidity", "Predicted Humidity"),
                      values = c("blue", "red"))


pcr_model <- pcr(luminosity~., data = data3,scale =TRUE, validation = "CV",segments=5)
pcr_pred <- predict(pcr_model, test3, ncomp =6)
y_test <- test3[, 1]
pcr_pred_numeric <- unlist(pcr_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pcr_pred_numeric) - as.numeric(y_test_numeric))^2))

combined_data3 <- rbind(data3, test3)
# Assuming combined_data is your combined dataframe and pcr_pred is your PCR predictions

# Number of predictions
num_predictions <- length(pcr_pred)

# Initialize the temp_pred column with NAs
combined_data3$lum_pred <- NA

# Replace the last num_predictions values in temp_pred with your PCR predictions
combined_data3[(nrow(combined_data3) - num_predictions + 1):nrow(combined_data3), "lum_pred"] <- pcr_pred

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
plot_data <- data.frame(Index = 1:nrow(combined_data3), 
                        Luminosity = combined_data3$luminosity, 
                        Lum_Pred = combined_data3$lum_pred)

# Generate the plot
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Luminosity, colour = "Actual Luminosity")) +
  geom_line(aes(y = Lum_Pred, colour = "Predicted Luminosity")) +
  theme_minimal() +
  labs(title = "Luminosity vs Predicted Luminosity", 
       x = "Index", 
       y = "Luminosity") +
  scale_colour_manual("", 
                      breaks = c("Actual Luminosity", "Predicted Luminosity"),
                      values = c("blue", "red"))


pcr_model <- pcr(atmospheric_pressure~., data = data4,scale =TRUE, validation = "CV",segments=5)
pcr_pred <- predict(pcr_model, test4, ncomp = 7)
y_test <- test4[, 1]
pcr_pred_numeric <- unlist(pcr_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pcr_pred_numeric) - as.numeric(y_test_numeric))^2))


combined_data4 <- rbind(data4, test4)
# Assuming combined_data is your combined dataframe and pcr_pred is your PCR predictions

# Number of predictions
num_predictions <- length(pcr_pred)

# Initialize the temp_pred column with NAs
combined_data4$temp_pred <- NA

# Replace the last num_predictions values in temp_pred with your PCR predictions
combined_data4[(nrow(combined_data4) - num_predictions + 1):nrow(combined_data4), "atm_pred"] <- pcr_pred

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
plot_data <- data.frame(Index = 1:nrow(combined_data4), 
                        atmospheric_pressure = combined_data4$atmospheric_pressure, 
                        atm_Pred = combined_data4$atm_pred)

# Generate the plot
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = atmospheric_pressure, colour = "Actual atmospheric_pressure")) +
  geom_line(aes(y = atm_Pred, colour = "Predicted atmospheric_pressure")) +
  theme_minimal() +
  labs(title = "atmospheric_pressure vs Predicted atmospheric_pressure", 
       x = "Index", 
       y = "atmospheric_pressure") +
  scale_colour_manual("", 
                      breaks = c("Actual atmospheric_pressure", "Predicted atmospheric_pressure"),
                      values = c("blue", "red"))

pls_model = plsr(temperature~., data = data1, scale =TRUE, validation = "CV")
pls_pred <- predict(pls_model, test1, ncomp =5)
y_test <- test1[, 1]
pls_pred_numeric <- unlist(pls_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pls_pred_numeric) - as.numeric(y_test_numeric))^2))

# Number of predictions
num_predictions <- length(pls_pred)

# Initialize the temp_pred column with NAs
combined_data1$temp_pred2 <- NA

# Replace the last num_predictions values in temp_pred with your PCR predictions
combined_data1[(nrow(combined_data1) - num_predictions + 1):nrow(combined_data1), "temp_pred2"] <- pls_pred

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
plot_data <- data.frame(Index = 1:nrow(combined_data1), 
                        Temperature = combined_data1$temperature, 
                        Temp_Pred = combined_data1$temp_pred)

# Generate the plot
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Temperature, colour = "Actual Temperature")) +
  geom_line(aes(y = Temp_Pred, colour = "Predicted Temperature")) +
  theme_minimal() +
  labs(title = "Temperature vs Predicted Temperature", 
       x = "Index", 
       y = "Temperature") +
  scale_colour_manual("", 
                      breaks = c("Actual Temperature", "Predicted Temperature"),
                      values = c("blue", "red"))



pls_model = plsr(humidity~., data = data2, scale =TRUE, validation = "CV")
pls_pred <- predict(pls_model, test2, ncomp = 7)
y_test <- test2[, 1]
pls_pred_numeric <- unlist(pls_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pls_pred_numeric) - as.numeric(y_test_numeric))^2))

combined_data2$hum_pred2 <- NA
combined_data2[(nrow(combined_data2) - num_predictions + 1):nrow(combined_data2), "hum_pred2"] <- pls_pred
library(ggplot2)
plot_data <- data.frame(Index = 1:nrow(combined_data2), 
                        Humidity = combined_data2$humidity, 
                        Hum_Pred = combined_data2$hum_pred2)

ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Humidity, colour = "Actual Humidity")) +
  geom_line(aes(y = Hum_Pred, colour = "Predicted Humidity")) +
  theme_minimal() +
  labs(title = "Humidity vs Predicted Humidity", 
       x = "Index", 
       y = "Humidity") +
  scale_colour_manual("", 
                      breaks = c("Actual Humidity", "Predicted Humidity"),
                      values = c("blue", "red"))

pls_model = plsr(luminosity~., data = data3, scale =TRUE, validation = "CV")
pls_pred <- predict(pls_model, test3, ncomp = 4)
y_test <- test3[, 1]
pls_pred_numeric <- unlist(pls_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pls_pred_numeric) - as.numeric(y_test_numeric))^2))

combined_data3$lum_pred2 <- NA
combined_data3[(nrow(combined_data3) - num_predictions + 1):nrow(combined_data3), "lum_pred2"] <- pls_pred
library(ggplot2)

plot_data <- data.frame(Index = 1:nrow(combined_data3), 
                        Luminosity = combined_data3$luminosity, 
                        Lum_Pred = combined_data3$lum_pred2)

ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Luminosity, colour = "Actual Luminosity")) +
  geom_line(aes(y = Lum_Pred, colour = "Predicted Luminosity")) +
  theme_minimal() +
  labs(title = "Luminosity vs Predicted Luminosity", 
       x = "Index", 
       y = "Luminosity") +
  scale_colour_manual("", 
                      breaks = c("Actual Luminosity", "Predicted Luminosity"),
                      values = c("blue", "red"))

pls_model = plsr(atmospheric_pressure~., data = data4, scale =TRUE, validation = "CV")
pls_pred <- predict(pls_model, test4, ncomp = 5)
y_test <- test4[, 1]
pls_pred_numeric <- unlist(pls_pred)
y_test_numeric <- unlist(y_test)
sqrt(mean((as.numeric(pls_pred_numeric) - as.numeric(y_test_numeric))^2))

num_predictions <- length(pls_pred)
# Initialize the temp_pred column with NAs
combined_data4$atm_pred2 <- NA

# Replace the last num_predictions values in temp_pred with your PCR predictions
combined_data4[(nrow(combined_data4) - num_predictions + 1):nrow(combined_data4), "atm_pred2"] <- pls_pred

# Load necessary library
library(ggplot2)

# Create a dataframe for plotting
plot_data <- data.frame(Index = 1:nrow(combined_data4), 
                        atmospheric_pressure = combined_data4$atmospheric_pressure, 
                        atm_Pred = combined_data4$atm_pred2)

# Generate the plot
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = atmospheric_pressure, colour = "Actual atmospheric_pressure")) +
  geom_line(aes(y = atm_Pred, colour = "Predicted atmospheric_pressure")) +
  theme_minimal() +
  labs(title = "atmospheric_pressure vs Predicted atmospheric_pressure", 
       x = "Index", 
       y = "atmospheric_pressure") +
  scale_colour_manual("", 
                      breaks = c("Actual atmospheric_pressure", "Predicted atmospheric_pressure"),
                      values = c("blue", "red"))

