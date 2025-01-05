# Load data
historic_dt <- read.csv('historic_property_data.csv')
predict_dt <- read.csv('predict_property_data.csv')

#-----------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#

# 1. Handling N/A





# View summary information of the data
str(historic_dt)
str(predict_dt)

# View basic statistics of the data
summary(historic_dt)
summary(predict_dt)

# Convert logical columns to factors
historic_dt$ind_large_home <- as.factor(historic_dt$ind_large_home)
historic_dt$ind_garage <- as.factor(historic_dt$ind_garage)
historic_dt$ind_arms_length <- as.factor(historic_dt$ind_arms_length)

predict_dt$ind_large_home <- as.factor(predict_dt$ind_large_home)
predict_dt$ind_garage <- as.factor(predict_dt$ind_garage)
predict_dt$ind_arms_length <- as.factor(predict_dt$ind_arms_length)

# Convert character columns to factors
convert_to_factor <- function(df) {
  for (col in names(df)) {
    # Check if the column is of character type
    if (is.character(df[[col]])) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  return(df)
}

# Apply the function to historic_dt and predict_dt
historic_dt <- convert_to_factor(historic_dt)
predict_dt <- convert_to_factor(predict_dt)

# Remove the 'ind_garage' column
historic_dt <- historic_dt[, !names(historic_dt) %in% "ind_garage"]
predict_dt <- predict_dt[, !names(predict_dt) %in% "ind_garage"]

# Check the transformation results
str(historic_dt)
str(predict_dt)

########## 1.1: Handling missing values ##########
# Count the number of missing values in each column
colSums(is.na(historic_dt))
colSums(is.na(predict_dt))

# Define a function to handle missing values
fill_na <- function(df) {
  for (col in names(df)) {
    # Replace missing values in numeric columns with the median
    if (is.numeric(df[[col]]) || is.integer(df[[col]])) {
      median_value <- median(df[[col]], na.rm = TRUE)
      df[[col]][is.na(df[[col]])] <- median_value
    }
    # Replace missing values in categorical or character columns with 'Unknown'
    else if (is.factor(df[[col]]) || is.character(df[[col]])) {
      if (is.factor(df[[col]])) {
        levels(df[[col]]) <- c(levels(df[[col]]), "Unknown")
      }
      df[[col]][is.na(df[[col]])] <- "Unknown"
    }
  }
  return(df)
}

# Apply the function to historic_dt and predict_dt
historic_dt <- fill_na(historic_dt)
predict_dt <- fill_na(predict_dt)

# Confirm the results
print("Missing values handled:")
colSums(is.na(historic_dt))
colSums(is.na(predict_dt))

# Define a function to adjust data types
adjust_data_types <- function(df) {
  # Specify columns to convert to categorical type
  categorical_columns <- c("meta_class", "meta_town_code", "meta_nbhd",
                           "char_ext_wall", "char_roof_cnst", "char_heat",
                           "char_use", "geo_fips")
  # Specify binary columns to convert to categorical type
  binary_columns <- c("geo_withinmr100", "geo_withinmr101300")
  
  # Convert specified columns to categorical type
  for (col in categorical_columns) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  # Convert binary columns to categorical with specific levels
  for (col in binary_columns) {
    if (col %in% names(df)) {
      df[[col]] <- factor(df[[col]], levels = c(0, 1), labels = c("Outside", "Inside"))
    }
  }
  return(df)
}

# Apply the function to historic_dt and predict_dt
historic_dt <- adjust_data_types(historic_dt)
predict_dt <- adjust_data_types(predict_dt)

# Confirm the results
str(historic_dt)
str(predict_dt)






#-----------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#






# Step2 : Variable Selection







#################################################
######### Selecting numeric variables #########
#############################################


# Define a function to calculate correlations and select meaningful variables
select_significant_numeric <- function(df, target_col, threshold = 0.4) {
  # Select only numeric and integer columns
  numeric_cols <- sapply(df, is.numeric) | sapply(df, is.integer)
  numeric_data <- df[, numeric_cols]
  
  # Calculate correlation matrix
  correlation_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Extract correlations with the target column
  target_correlation <- correlation_matrix[, target_col, drop = FALSE]
  
  # Select variables with correlation above the threshold
  significant_vars <- rownames(target_correlation)[abs(target_correlation[, 1]) >= threshold]
  
  # Return the results
  return(significant_vars)
}

# Get significant numeric variables
significant_numeric_cols <- select_significant_numeric(historic_dt, target_col = "sale_price", threshold = 0.4)

# Print the results
print("Significant numeric variables:")
print(significant_numeric_cols)






###################################################
######### Selecting categorical variables #########
##################################################

# Define a function to select meaningful categorical variables
select_significant_categorical <- function(df, target_col, categorical_columns, threshold = 0.05) {
  significant_columns <- c()
  
  # Perform ANOVA for each categorical variable
  for (col in categorical_columns) {
    if (col %in% names(df)) {
      # ANOVA test
      anova_result <- aov(df[[target_col]] ~ as.factor(df[[col]]))
      p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]  # Extract p-value
      
      # Add to significant columns if p-value is below the threshold
      if (p_value < threshold) {
        significant_columns <- c(significant_columns, col)
      }
    }
  }
  return(significant_columns)
}

# Display data types
data_types <- data.frame(Column = names(historic_dt), Type = sapply(historic_dt, class))
print(data_types)

# Define categorical variables
categorical_columns <- c("meta_class", "meta_town_code", "meta_nbhd", "meta_cdu",
                         "meta_deed_type", "char_ext_wall", "char_roof_cnst", "char_heat", "char_use",
                         "geo_property_city", "geo_property_zip", "geo_municipality", "geo_fips",
                         "geo_school_elem_district", "geo_school_hs_district",
                         "geo_withinmr100", "geo_withinmr101300", "ind_large_home", "ind_arms_length")

# Execute the function
significant_categorical_cols <- select_significant_categorical(historic_dt,
                                                               target_col = "sale_price",
                                                               categorical_columns = categorical_columns,
                                                               threshold = 0.05)

# Print the results
print("Significant categorical variables:")
print(significant_categorical_cols)

# Combine selected numeric and categorical variables
final_selected_columns <- c(significant_numeric_cols, significant_categorical_cols)
historic_sel_dt <- historic_dt[final_selected_columns]





#-----------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#





# Step3 : ML develope





#####################################
######### Linear Regression #########
#####################################
# Load the required package
library(caret)

# Split the data: 80% training, 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(historic_sel_dt$sale_price, p = 0.8, list = FALSE)
train_data <- historic_sel_dt[train_index, ]
test_data <- historic_sel_dt[-train_index, ]

# Train a linear regression model using caret
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)  # Cross-validation settings
lm_model <- train(
  sale_price ~ .,           # Dependent variable and predictors
  data = train_data,        # Training dataset
  method = "lm",            # Linear regression method
  trControl = train_control # Cross-validation control
)

# View the model summary
print(lm_model)

# Predict on test data
predictions_lm <- predict(lm_model, newdata = test_data)

# Evaluate model performance (RMSE and R²)
actuals_lm <- test_data$sale_price
rmse_lm <- sqrt(mean((actuals_lm - predictions_lm)^2))  # Calculate RMSE
cat("Linear regression model test RMSE:", rmse_lm, "\n")

# Calculate R² (coefficient of determination)
sse <- sum((actuals_lm - predictions_lm)^2)  # Sum of squared errors
sst <- sum((actuals_lm - mean(actuals_lm))^2)  # Total variance
r2 <- 1 - (sse / sst)  # R² calculation
cat("Linear regression model test R²:", r2, "\n")

### Scaling
library(caret)

# Select predictors (excluding the target variable 'sale_price')
predictors <- setdiff(names(train_data), "sale_price")

# Standardize predictors in the training data
preProc <- preProcess(train_data[, predictors], method = c("center", "scale"))
train_data_scaled <- train_data
train_data_scaled[, predictors] <- predict(preProc, train_data[, predictors])

# Apply the same transformation to the test data
test_data_scaled <- test_data
test_data_scaled[, predictors] <- predict(preProc, test_data[, predictors])

# Train a linear regression model on the scaled data
set.seed(123)
train_control <- trainControl(method = "cv", number = 5)  # Cross-validation settings
lm_model_scaled <- train(
  sale_price ~ .,
  data = train_data_scaled,
  method = "lm",
  trControl = train_control
)

# Predict on the standardized test data
predictions_lm_scaled <- predict(lm_model_scaled, newdata = test_data_scaled)

# Calculate RMSE on the original scale
actuals_original <- test_data$sale_price  # Actual values remain on the original scale
rmse_original_scale <- sqrt(mean((actuals_original - predictions_lm_scaled)^2))  # RMSE
cat("Original scale RMSE:", rmse_original_scale, "\n")







#####################################
############## Lasso ##############
#####################################
library(glmnet)

# Split the data: 80% training, 20% testing
set.seed(123)
train_index <- createDataPartition(historic_sel_dt$sale_price, p = 0.8, list = FALSE)
train_data <- historic_sel_dt[train_index, ]
test_data <- historic_sel_dt[-train_index, ]

# Perform one-hot encoding (convert all variables to numeric)
x_train <- model.matrix(sale_price ~ ., train_data)[, -1]  # Remove the intercept
y_train <- train_data$sale_price

x_test <- model.matrix(sale_price ~ ., test_data)[, -1]
y_test <- test_data$sale_price

# Train a Lasso model
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "gaussian", nfolds = 20)

# Get the optimal lambda value
best_lambda <- lasso_model$lambda.min
cat("Optimal lambda value for Lasso:", best_lambda, "\n")

# Train the model with the optimal lambda
final_lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Predict on test data
predictions_lasso <- predict(final_lasso_model, newx = x_test)

# Calculate RMSE
rmse_lasso <- sqrt(mean((y_test - predictions_lasso)^2))
cat("Lasso regression test RMSE:", rmse_lasso, "\n")







#####################################
######### Elastic Net #########
#####################################
library(caret)

# Split the data: 80% training, 20% testing
set.seed(123)  # Set seed for reproducibility
train_index <- createDataPartition(historic_sel_dt$sale_price, p = 0.8, list = FALSE)
train_data <- historic_sel_dt[train_index, ]
test_data <- historic_sel_dt[-train_index, ]

# Define independent (X) and dependent (y) variables
x_train <- model.matrix(sale_price ~ ., train_data)[, -1]  # Remove intercept
y_train <- train_data$sale_price

x_test <- model.matrix(sale_price ~ ., test_data)[, -1]
y_test <- test_data$sale_price

# Train an Elastic Net model (alpha = 0.5 balances Ridge and Lasso)
set.seed(123)
elastic_model <- cv.glmnet(
  x_train, y_train,
  alpha = 0.5,  # Elastic Net mixing ratio
  family = "gaussian",  # Regression problem
  nfolds = 10  # 10-fold cross-validation
)

# Find the optimal lambda value
best_lambda <- elastic_model$lambda.min
cat("Optimal lambda value for Elastic Net:", best_lambda, "\n")

# Train the final Elastic Net model with the optimal lambda
final_elastic_model <- glmnet(x_train, y_train, alpha = 0.5, lambda = best_lambda)

# Predict on test data
predictions_ela <- predict(final_elastic_model, newx = x_test)

# Evaluate performance (calculate RMSE)
rmse_ela <- sqrt(mean((y_test - predictions_ela)^2))
cat("Elastic Net regression test RMSE:", rmse_ela, "\n")

# Calculate R²
sse <- sum((y_test - predictions_ela)^2)  # Sum of squared errors
sst <- sum((y_test - mean(y_test))^2)  # Total variance
r2 <- 1 - (sse / sst)  # R² calculation
cat("Elastic Net regression R²:", r2, "\n")







#####################################
############### GBM ###############
#####################################
library(gbm)
library(caret)

# Split the data: 80% training, 20% testing
set.seed(123)
train_index <- createDataPartition(historic_sel_dt$sale_price, p = 0.8, list = FALSE)
train_data <- historic_sel_dt[train_index, ]
test_data <- historic_sel_dt[-train_index, ]

# Train a GBM model
set.seed(123)
gbm_model <- gbm(
  formula = sale_price ~ .,       # Dependent variable and predictors
  distribution = "gaussian",      # Regression problem
  data = train_data,              # Training dataset
  n.trees = 500,                  # Number of trees
  interaction.depth = 5,          # Depth of trees
  shrinkage = 0.1,                # Learning rate
  cv.folds = 10,                  # 10-fold cross-validation
  verbose = FALSE                 # Suppress training output
)

# Find the optimal number of trees
best_trees <- gbm.perf(gbm_model, method = "cv")
cat("Optimal number of trees:", best_trees, "\n")

# Predict on test data
gbm_predictions <- predict(gbm_model, test_data, n.trees = best_trees)

# Calculate RMSE
gbm_rmse <- sqrt(mean((test_data$sale_price - gbm_predictions)^2))
cat("GBM regression test RMSE:", gbm_rmse, "\n")





#####################################
############### SVM ###############
#####################################

# Load required packages
library(e1071)
library(caret)

# Split the data: 80% training, 20% testing
set.seed(123)
train_index <- createDataPartition(historic_sel_dt$sale_price, p = 0.8, list = FALSE)
train_data <- historic_sel_dt[train_index, ]
test_data <- historic_sel_dt[-train_index, ]

# Train an SVM model (using radial kernel)
set.seed(123)
svm_model <- svm(
  sale_price ~ .,        # Dependent variable and predictors
  data = train_data,     # Training dataset
  kernel = "radial",     # RBF (Radial Basis Function) kernel
  cost = 1,              # Regularization parameter
  gamma = 0.1            # Gamma value for the RBF kernel
)

# Display model summary
print(svm_model)

# Predict on test data
predictions_svm <- predict(svm_model, newdata = test_data)

# Calculate RMSE
actuals_svm <- test_data$sale_price
rmse_svm <- sqrt(mean((actuals_svm - predictions_svm)^2))
cat("SVM regression test RMSE:", rmse_svm, "\n")





#####################################
########## Random Forest ##########
#####################################
# Load required packages
library(randomForest)
library(caret)

# Split the data: 70% training, 30% testing
set.seed(123)
train_index_dynamic <- createDataPartition(historic_sel_dt$sale_price, p = 0.7, list = FALSE)
train_data_dynamic <- historic_sel_dt[train_index_dynamic, ]
test_data_dynamic <- historic_sel_dt[-train_index_dynamic, ]

# Apply one-hot encoding to training and test datasets
dummies_train_dynamic <- model.matrix(sale_price ~ ., data = train_data_dynamic)[, -1]  # Remove intercept
dummies_test_dynamic <- model.matrix(sale_price ~ ., data = test_data_dynamic)[, -1]  # Remove intercept

# Train a Random Forest model with dynamic ntree adjustment
set.seed(123)
rmse_values <- c()  # Store RMSE for each ntree value
improvement_threshold <- 0.01  # Threshold for improvement
previous_rmse <- Inf  # Initialize previous RMSE
best_ntree <- 0  # Store optimal ntree value

for (ntree_dynamic in seq(10, 100, by = 10)) {
  rf_model_dynamic <- randomForest(
    x = dummies_train_dynamic,
    y = train_data_dynamic$sale_price,
    ntree = ntree_dynamic,
    importance = TRUE
  )
  
  # Predict on test data
  predictions_rf_dynamic <- predict(rf_model_dynamic, newdata = dummies_test_dynamic)
  
  # Calculate RMSE
  rmse_rf_dynamic <- sqrt(mean((test_data_dynamic$sale_price - predictions_rf_dynamic)^2))
  rmse_values <- c(rmse_values, rmse_rf_dynamic)
  
  # Check for improvement
  improvement <- previous_rmse - rmse_rf_dynamic
  if (improvement > 0) {
    previous_rmse <- rmse_rf_dynamic
    best_ntree <- ntree_dynamic
  } else if (improvement < improvement_threshold) {
    cat("Performance improvement stopped - Optimal ntree:", best_ntree, "\n")
    break
  }
}

# Train the final model with the optimal ntree value
cat("Optimal ntree value:", best_ntree, "\n")
final_rf_model_dynamic <- randomForest(
  x = dummies_train_dynamic,
  y = train_data_dynamic$sale_price,
  ntree = best_ntree,
  importance = TRUE
)


# Predict on test data using the final model
final_predictions_rf_dynamic <- predict(final_rf_model_dynamic, newdata = dummies_test_dynamic)
final_rmse_rf_dynamic <- sqrt(mean((test_data_dynamic$sale_price - final_predictions_rf_dynamic)^2))
cat("Random Forest regression test RMSE (Optimal ntree):", final_rmse_rf_dynamic, "\n")

# Extract and display variable importance scores
importance_scores_dynamic <- importance(final_rf_model_dynamic)
print("Random Forest variable importance (based on optimal ntree):")
print(importance_scores_dynamic)




#-----------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#


# Step4 : prediction


# One-hot encode predict_dt for prediction
dummies_predict <- model.matrix(~ ., data = predict_dt)[, -1]  # Remove intercept

# Align columns between train and predict datasets
missing_columns <- setdiff(colnames(dummies_train_dynamic), colnames(dummies_predict))
for (col in missing_columns) {
  dummies_predict <- cbind(dummies_predict, 0)  # Add missing columns with default value 0
  colnames(dummies_predict)[ncol(dummies_predict)] <- col
}

# Remove extra columns not in train data
dummies_predict <- dummies_predict[, colnames(dummies_predict) %in% colnames(dummies_train_dynamic)]

# Predict sale_price for predict_dt
predictions_for_predict_dt <- predict(final_rf_model_dynamic, newdata = dummies_predict)

# Add predictions to predict_dt
predict_dt$predicted_sale_price <- predictions_for_predict_dt

# Display prediction results
print("Predicted sale_price for predict_dt:")
print(head(predict_dt))

# Save predictions to a CSV file
write.csv(predict_dt, "predicted_sale_prices.csv", row.names = FALSE)
cat("Predictions saved to 'predicted_sale_prices.csv'\n")

# Format and display final results
final_result <- predict_dt[c("pid", "predicted_sale_price")]
colnames(final_result) <- c("pid", "assessed_value")
final_result

