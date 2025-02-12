# Load required libraries
library(tidyverse)
library(car)
library(ROCR)
library(pROC)
library(ResourceSelection)
library(MASS) 
library(mgcv)
library(ggplot2)

# Load data
hospital <- read.table("hospital.txt", header = TRUE)

# Initial exploration
head(hospital)  # View the first few rows of the dataset
table(hospital$died)  # Count of deaths and survivors
table(hospital$los)   # Distribution of length of stay (los)

# Logistic regression model for mortality
model_mortality <- glm(died ~ ., family = binomial(link = "logit"), 
                       data = hospital)

# Model summary
summary(model_mortality)

# Stepwise AIC to remove redundant variables
model_mortality_clean <- stepAIC(model_mortality)
summary(model_mortality_clean)

# Exponentiate coefficients for interpretation
coefficients <- coef(model_mortality_clean)
exp_coefficients <- exp(coefficients)

# Print exponentiated coefficients with labels
for (name in names(exp_coefficients)) {
  cat(name, ":", exp_coefficients[name], "\n")
}

# Check for non-linear relationships using GAM
model_gam <- gam(died ~ s(los) + s(age) + gender + s(bmi) + 
                  s(respiratory) + s(sp02) + avpu + risk, 
                 family = binomial(link = "logit"), 
                 data = hospital)
summary(model_gam)

# Plot GAM results 
# Variability in smooth terms indicates returning to stepwise model
par(mfrow = c(1, 1))  # Reset plotting layout
plot(model_gam, shade = TRUE)  

# Interaction terms in logistic regression
# Example 1: Interaction between age and severity
model_with_interactions <- glm(died ~ los + age + gender + bmi + sp02 + 
                                respiratory + avpu + risk + age:severity,
                               family = binomial(link = "logit"), 
                               data = hospital)
summary(model_with_interactions)

# Example 2: Interaction between BMI and risk
model_with_interactions2 <- glm(died ~ los + age + gender + bmi + sp02 + 
                                respiratory + avpu + risk + bmi:risk,
                                family = binomial(link = "logit"), 
                                data = hospital)
summary(model_with_interactions2)

# Example 3: Interaction between age and risk
model_with_interactions3 <- glm(died ~ los + age + gender + bmi + sp02 + 
                                respiratory + avpu + risk + age:risk,
                                family = binomial(link = "logit"), 
                                data = hospital)
summary(model_with_interactions3)

# No meaningful results for interactions -> return to model_mortality_clean

# Check multicollinearity with VIF
vif(model_mortality_clean)

# Generate predicted probabilities from the clean model
predicted_probs <- predict(model_mortality_clean, type = "response")

# Evaluate model performance at threshold = 0.5
threshold_1 <- 0.5
predicted_classes_1 <- ifelse(predicted_probs > threshold_1, 1, 0)

# Confusion matrix for threshold = 0.5
conf_matrix_1 <- table(Predicted = predicted_classes_1, Actual = hospital$died)
print("Confusion Matrix (Threshold = 0.5):")
print(conf_matrix_1)

# Extract metrics from confusion matrix
TP_1 <- conf_matrix_1[2, 2]  # True Positives
TN_1 <- conf_matrix_1[1, 1]  # True Negatives
FP_1 <- conf_matrix_1[2, 1]  # False Positives
FN_1 <- conf_matrix_1[1, 2]  # False Negatives

# Calculate accuracy, sensitivity, and false positive rate
accuracy_1 <- (TP_1 + TN_1) / sum(conf_matrix_1)
TPR_1 <- TP_1 / (TP_1 + FN_1)  # Sensitivity
FPR_1 <- FP_1 / (FP_1 + TN_1)  # False Positive Rate

# Display metrics
cat(sprintf("Threshold 0.5 - Accuracy: %.3f\n", accuracy_1))
cat(sprintf("Threshold 0.5 - Sensitivity: %.3f\n", TPR_1))
cat(sprintf("Threshold 0.5 - False Positive Rate: %.3f\n", FPR_1))

# ROC curve and AUC
roc_curve <- roc(hospital$died, predicted_probs)
auc_value <- auc(roc_curve)

# Plot the ROC curve
plot(
  roc_curve,
  main = "ROC Curve (Threshold = 0.026)",  # Main title
  col = "blue",  # Color of the ROC curve
  lwd = 2,       # Line width
  xlab = "False Positive Rate", 
  ylab = "True Positive Rate", 
  print.auc = TRUE,  # Add AUC to the plot
  print.auc.y = 0.4  # Position of AUC display
)
abline(a = 0, b = 1, col = "gray", lty = 2)  # Add diagonal reference line

# Optimal threshold from ROC curve
optimal_threshold <- coords(roc_curve, "best", ret = "threshold")
cat(sprintf("Optimal Threshold: %.3f\n", optimal_threshold))

# Evaluate model at optimal threshold
predicted_classes_optimal <- ifelse(predicted_probs > optimal_threshold, 1, 0)
conf_matrix_optimal <- table(Predicted = predicted_classes_optimal, 
                             Actual = hospital$died)
print("Confusion Matrix (Optimal Threshold):")
print(conf_matrix_optimal)

# Metrics for optimal threshold
TP_opt <- conf_matrix_optimal[2, 2]  # True Positives
TN_opt <- conf_matrix_optimal[1, 1]  # True Negatives
FP_opt <- conf_matrix_optimal[2, 1]  # False Positives
FN_opt <- conf_matrix_optimal[1, 2]  # False Negatives

accuracy_opt <- (TP_opt + TN_opt) / sum(conf_matrix_optimal)
TPR_opt <- TP_opt / (TP_opt + FN_opt)  # Sensitivity
FPR_opt <- FP_opt / (FP_opt + TN_opt)  # False Positive Rate

# Display metrics for optimal threshold
cat(sprintf("Optimal Threshold - Accuracy: %.3f\n", accuracy_opt))
cat(sprintf("Optimal Threshold - Sensitivity: %.3f\n", TPR_opt))
cat(sprintf("Optimal Threshold - False Positive Rate: %.3f\n", FPR_opt))

### Length of Stay (LOS) Model ###

# Check mean and variance of response variable
mean_los <- mean(hospital$los)
var_los <- var(hospital$los)
cat(sprintf("Mean LOS: %.2f\n", mean_los))
cat(sprintf("Variance LOS: %.2f\n", var_los))

# Fit Negative Binomial model
model_los <- glm.nb(los ~ ., data = hospital)
summary(model_los)

# Stepwise AIC for Negative Binomial model
model_los_clean <- stepAIC(model_los)
summary(model_los_clean)

# Exponentiate coefficients for interpretation
exp_coefficients <- exp(coef(model_los_clean))
print(exp_coefficients)

# Check for non-linear relationships using GAM
model_los_gam <- gam(los ~ died + gender + severity + s(dbp) + s(temp), 
                     family = nb(), 
                     data = hospital)
summary(model_los_gam)

# Testing interaction terms for LOS model
interaction_models <- list(
  glm.nb(los ~ died + gender + severity + dbp + temp + severity:gender, 
         data = hospital),
  glm.nb(los ~ died + gender + severity + dbp + temp + died:temp, 
         data = hospital),
  glm.nb(los ~ died + gender + severity + dbp + temp + gender:age, 
         data = hospital))

# Iterate over models to display summaries
for (i in seq_along(interaction_models)) {
  cat(sprintf("\nInteraction Model %d Summary:\n", i))
  print(summary(interaction_models[[i]]))
}

# Interaction terms did not improve the model -> return to model_los_clean

# Check multicollinearity
vif(model_los_clean)

# Residual analysis for LOS model
par(mfrow = c(2, 2))
plot(model_los_clean)

# Exclude influential points
hospital_cleaned <- hospital[!rownames(hospital) %in% c("609", "166", 
                                                        "719", "447"), ]
model_filtered <- glm.nb(los ~ died + gender + severity + dbp + temp, 
                         data = hospital_cleaned)
summary(model_filtered)

par(mfrow = c(2, 2))
plot(model_filtered)
