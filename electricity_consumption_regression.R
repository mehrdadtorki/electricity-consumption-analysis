# electricity_consumption_regression.R
# -------------------------------------
# This script performs regression analysis on electricity consumption data
# using R. It covers data import, preprocessing, model building, prediction,
# and visualization.

# Load required library
# readxl is used to read Excel files (.xlsx)
library(readxl)   

# Import dataset
# Replace the path with the location of your dataset.
# NOTE: For portability, it's better to keep your dataset in the project folder
# and refer to it with a relative path (e.g., "data/Electricity_Consumption10.xlsx")
data <- read_excel("C:/Users/ariyana/Downloads/Regression_Datasets_All/Electricity_Consumption10.xlsx")

# Display the first few rows of the dataset
# This helps you verify that the data was loaded correctly
head(data) 

# Show the structure of the dataset
# str() displays data types and format of each column
str(data)

# Show summary statistics of the dataset
# This provides min, max, mean, median, etc. for numeric variables,
# and frequency counts for categorical variables
summary(data)


# ------------------------------
# Data preprocessing
# ------------------------------

# Convert categorical variables to factors
# Factors are necessary for regression models when dealing with categorical data
data$Temperature <- as.factor(data$Temperature)
data$Humidity <- as.factor(data$Humidity)
data$Day_of_Week <- as.factor(data$Day_of_Week)
data$Appliance_Usage <- as.factor(data$Appliance_Usage)

# Re-check dataset structure to confirm changes
str(data)


# ------------------------------
# Model building
# ------------------------------

# Fit a linear regression model
# Target variable: Target
# Predictors: Temperature, Humidity, Day_of_Week, Time_of_Day, Appliance_Usage
model <- lm(Target ~ Temperature + Humidity + Day_of_Week + Time_of_Day + Appliance_Usage, data = data)

# View model summary
# Provides coefficients, R-squared, p-values, and other details
summary(model)

# Display only model coefficients
coef(model)   # Model coefficients
summary(model)$coefficients  # Detailed coefficients with standard error, t-value, p-value


# ------------------------------
# Prediction
# ------------------------------

# Predict target values using the fitted model
pred <- predict(model, data)

# Show first few predicted values
head(pred)


# ------------------------------
# Visualization
# ------------------------------

# Plot actual vs predicted values
# Helps visualize model performance
plot(data$Target, pred, 
     xlab = "Actual Target", 
     ylab = "Predicted Target", 
     main = "Actual vs Predicted")

# Add a reference line (y = x) in red
# If predictions were perfect, all points would lie on this line
abline(0, 1, col = "red")
