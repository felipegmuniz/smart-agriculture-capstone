# choose_your_own_script.R
# Crop Recommendation using Soil & Climate Data

# Automatically install and load required packages
required_packages <- c("tidyverse", "caret", "randomForest", "nnet")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Check if dataset exists, otherwise stop
if (!file.exists("Crop_recommendation.csv")) {
  stop("Dataset not found. Please place 'Crop_recommendation.csv' in the working directory.")
}

# Load data
df <- read.csv("Crop_recommendation.csv")
df$label <- as.factor(df$label)

# Train-test split
set.seed(123)
index <- createDataPartition(df$label, p = 0.8, list = FALSE)
train <- df[index, ]
test <- df[-index, ]

# Logistic Regression (multinomial)
log_model <- multinom(label ~ ., data = train)
log_pred <- predict(log_model, test)
log_acc <- mean(log_pred == test$label)

# Random Forest
rf_model <- randomForest(label ~ ., data = train)
rf_pred <- predict(rf_model, test)
rf_acc <- mean(rf_pred == test$label)

# Print accuracies
cat("Logistic Regression Accuracy:", round(log_acc, 4), "\n")
cat("Random Forest Accuracy:", round(rf_acc, 4), "\n")
