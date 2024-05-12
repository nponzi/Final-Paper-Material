#clear the memory
rm(list = ls())

setwd("/Users/nickponzi/lab/data")
library(tidyverse)

# Load the CSV file into a data frame
data <- read.csv("diabetes.csv", header = TRUE)

# Display the structure of the dataset
str(data)

# Display the first few rows of the dataset
head(data)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Calculate Z-scores for each numerical feature
z_scores <- scale(data[, -9]) # Exclude the outcome variable

# Identify outliers based on Z-scores
outliers <- apply(abs(z_scores) > 3, 1, any) # Check if any Z-score exceeds 3 for each row
outlier_indices <- which(outliers)

# Print the indices of outliers
print(outlier_indices)

# Remove outliers
data_clean <- data[-outlier_indices, ]

# Verify the removal of outliers
print("Number of observations after removing outliers:")
print(dim(data_clean))

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets
library(caTools)
split <- sample.split(data_clean$Outcome, SplitRatio = 0.7)
training_data <- subset(data_clean, split == TRUE)
testing_data <- subset(data_clean, split == FALSE)

# Check dimensions of training and testing sets
dim(training_data)
dim(testing_data)

#Create a logistic Regression Model

# Load required library
library(glmnet)

# Create a logistic regression model
logistic_model <- glm(Outcome ~ ., data = training_data, family = "binomial")

# Print the summary of the model
summary(logistic_model)

#Evaluate Model Performance

# Predict probabilities of diabetes on testing data
logistic_probabilities <- predict(logistic_model, newdata = testing_data, type = "response")

# Convert probabilities to binary predictions (0 or 1) based on a threshold of 0.5
logistic_predictions <- ifelse(logistic_probabilities > 0.5, 1, 0)

# Calculate confusion matrix
conf_matrix <- table(Actual = testing_data$Outcome, Predicted = logistic_predictions)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Calculate precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])

# Calculate recall (sensitivity)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Calculate F1-score
f1_score <- 2 * precision * recall / (precision + recall)

# Print evaluation metrics
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall (Sensitivity):", recall))
print(paste("F1-Score:", f1_score))

