# ALY 6015 Final Project.

# Load all the library necessary in this project.
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(ggplot2)
library(stats)
library(psych)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(broom)
library(Matrix)
library(ISLR)
library(lattice)
library(glmnet)
library(caret)
library(Matrix)


# Prepare the data for analysis.
#  Load the dataset and view the data structure.
data <- read.csv("Life-Expectancy-Data-Updated.csv")
str(data)

# Rename the column.
data <- rename(data, Economy_status = Economy_status_Developed)

# Combine the teenager thinness columns together, then drop the original columns.
data <- data %>% mutate(thinness = Thinness_ten_nineteen_years + Thinness_five_nine_years)
data <- data[ , -c(16, 17, 20)]

# Convert 'Economy_status' to a factor.
data$Economy_status <- as.factor(data$Economy_status)

# Descriptive statistics.
summary(data)

# Data Visualization.
# Distribution of Life Expectancy.
hist(data$Life_expectancy, main = "Distribution of Life Expectancy", 
     xlab = "Life Expectancy", ylab = "Frequency")

# Distribution of Adult Mortality.
boxplot(data$Under_five_deaths, main = "Distribution of Children Under 5 Deaths", 
        xlab = "Children Under 5 Deaths", ylab = "Deaths of Children Under 5 Years Old per 1000 Population")

# Correlation Analysis
# Select all the numeric columns.
num_data <- data %>% select(where(is.numeric))

# Correlation Matrix for numeric variables.
cor <- cor(num_data, use = "pairwise")
cor

# Plot the correlation matrix.
cor_plot <- corrplot(cor, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")
cor_plot

# Analysis
# Q1: Does a country’s development status (Developed vs. Developing) impact life expectancy?
# Method: T-test (since we’re comparing mean life expectancy across two groups)

# Boxplot to visualize life expectancy by development status
ggplot(data, aes(x = Economy_status, y = Life_expectancy, fill = Economy_status)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Life Expectancy by Development Status", x = "Development Status", y = "Life Expectancy")

# State the hypothesis.
# H0: There is no significant difference in mean life expectancy between developed and developing countries.
# H1: There is a significant difference in mean life expectancy between developed and developing countries.

# Perform T-test (if only 2 groups).
t_test_result <- t.test(Life_expectancy  ~ Economy_status, data = data)
t_test_result

# Make a decision
if (t_test_result$p.value < 0.05) {
    print("Reject the null hypothesis: There is a difference in mean life expectancy")
} else {
    print("Fail to reject the null hypothesis: There is no difference in mean life expectancy")
}

# Q2: Which factors significantly influence life expectancy?
# Method: Multiple Linear Regression

# Run multiple linear regression
model <- lm(Life_expectancy ~ Adult_mortality + GDP_per_capita + Schooling + Alcohol_consumption + BMI, data = data)
summary(model)

# Plot regression diagnostics.
par(mfrow = c(2, 2))
plot(model)
dev.off()

# Check for multicollinearity (independent variables are not correlated).
vif(model)
vif(model) > 5 # Problem?

# Q3. Is there an association between a country’s development status and child mortality?
# Method: Chi-Square Test for Independence

# Create categorical variable for Child Mortality (High/Low based on median)
median_child_mortality <- median(data$Under_five_deaths, na.rm = TRUE)
median_child_mortality
data$Child_Mortality_Category <- ifelse(data$Under_five_deaths > median_child_mortality, "High", "Low")

# Convert variables to factors
data$Child_Mortality_Category <- as.factor(data$Child_Mortality_Category)
summary(data$Child_Mortality_Category)

# Create a contingency table
table_status_mortality <- table(data$Economy_status, data$Child_Mortality_Category)
table_status_mortality

# Perform Chi-Square Test
chi_test_result <- chisq.test(table_status_mortality)
chi_test_result

# Make a decision
if (chi_test_result$p.value < 0.05) {
    print("Reject the null hypothesis: Status and child mortality are associated.")
} else {
    print("Fail to reject the null hypothesis: Status and child mortality are independent.")
}

# Q4. Can we predict whether a country has high or low life expectancy based on health and economic factors?
# Method: Logistic Regression
L_data <- num_data

# Create categorical variable for Life Expectancy (High/Low based on median)
median_life_expectancy <- median(L_data$Life_expectancy, na.rm = TRUE)
median_life_expectancy
L_data$Life_Expectancy_Category <- ifelse(L_data$Life_expectancy > median_life_expectancy, "High", "Low")

# Convert to factor
L_data$Life_Expectancy_Category <- as.factor(L_data$Life_Expectancy_Category)
summary(L_data$Life_Expectancy_Category)

# Run Logistic Regression
logistic_model <- glm(Life_Expectancy_Category ~ BMI + Schooling + Population_mln +
                          Alcohol_consumption, data = L_data, family = binomial)
summary(logistic_model)

# Predicted probabilities
Predicted_Probability <- predict(logistic_model,newdata = L_data, type = "response")
# Convert probabilities to class labels
Predicted_Class <- as.factor(ifelse(Predicted_Probability >= 0.5, "High", "Low"))
# Confusion Matrix
confusionMatrix(Predicted_Class, L_data$Life_Expectancy_Category, positive = "High")

# Plot the Receiver Operating Characteristic (ROC) curve.
library(pROC)
ROC <- roc(L_data$Life_Expectancy_Category, as.numeric(Predicted_Probability))
plot(ROC, col = "blue", main="ROC Curve", ylab = "Sensitivity - TP Rate", xlab = "Specificity - FP Rate")

# Calculate and Interpret AUC.
auc_value <- auc(ROC)
print(paste("AUC:", auc_value))


# Q5. Does adding Ridge/LASSO Regularization improve the predictive power of our model?
# Method: Regularization (Ridge & Lasso)

# Split into training and test sets
set.seed(123)
train_index <- createDataPartition(num_data$Life_expectancy, p = 0.7, list = FALSE)
train_data <- num_data[train_index, ]
test_data <- num_data[-train_index, ]

# Prepare data for Ridge and LASSO
x_train <- model.matrix(Life_expectancy ~ ., train_data)[, -1]
head(x_train, 4)
y_train <- train_data$Life_expectancy
y_train

x_test <- model.matrix(Life_expectancy ~ ., test_data)[, -1]
head(x_test, 4)
y_test <- test_data$Life_expectancy
y_test

# Ridge Regression
# Use coross validation function to identify optimal lambda.
set.seed(123)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
cv_ridge
lambda_min_ridge <- cv_ridge$lambda.min
lambda_min_ridge

# Plot cross validation results
plot(cv_ridge)

# Fit Ridge model
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda_min_ridge)
ridge_model
ridge_coefs <- coef(ridge_model)
ridge_coefs

# Predict and calculate RMSE for Ridge
ridge_train_preds <- predict(ridge_model, s = lambda_min_ridge, newx = x_train)
ridge_train_rmse <- sqrt(mean((y_train - ridge_train_preds)^2))
ridge_train_rmse

ridge_test_preds <- predict(ridge_model, s = lambda_min_ridge, newx = x_test)
ridge_test_rmse <- sqrt(mean((y_test - ridge_test_preds)^2))
ridge_test_rmse

# LASSO Regression
set.seed(123)
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
cv_lasso
lambda_1se_lasso <- cv_lasso$lambda.1se
lambda_1se_lasso

# Plot LASSO results
plot(cv_lasso)
title("LASSO Regression Cross-Validation Plot")

# Fit LASSO model
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_1se_lasso)
lasso_model
lasso_coefs <- coef(lasso_model)
lasso_coefs

# Predict and calculate RMSE for LASSO
lasso_train_preds <- predict(lasso_model, s = lambda_1se_lasso, newx = x_train)
lasso_train_rmse <- sqrt(mean((y_train - lasso_train_preds)^2))
lasso_train_rmse

lasso_test_preds <- predict(lasso_model, s = lambda_1se_lasso, newx = x_test)
lasso_test_rmse <- sqrt(mean((y_test - lasso_test_preds)^2))
lasso_test_rmse







