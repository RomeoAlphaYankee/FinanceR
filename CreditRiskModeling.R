library(gmodels)
library(rpart)
library(rpart.plot)
library(pROC)
data(loan_data)

str(loan_data)

# Credit data exploration
# Examine prop table
CrossTable(loan_data$loan_status)
CrossTable(loan_data$grade, loan_data$loan_status, prop.r = TRUE, prop.c = F, prop.chisq = F, prop.t = F)

# Check data for outliers
summary(loan_data)
hist(loan_data$emp_length)
boxplot(loan_data$emp_length, horizontal = TRUE)

plot(loan_data$emp_length, loan_data$annual_inc)

hist(loan_data$int_rate)
hist(loan_data$annual_inc, breaks = sqrt(nrow(loan_data)))

boxplot(loan_data$annual_inc, horizontal = TRUE)
summary(loan_data$annual_inc)

# Appears to be at least one income outlier
# Use 3rd quartile + 1.5 X interquartile range as a rule of thumb for cutoff
outlier_cutoff <- quantile(loan_data$annual_inc, 0.75) + 1.5 * IQR(loan_data$annual_inc)
index_outiler_ROT <- which(loan_data$annual_inc > outlier_cutoff)
loan_data_ROT <- loan_data$annual_inc[-index_outiler_ROT]

hist(loan_data_ROT, breaks = sqrt(nrow(loan_data)))

# Check loan amount for outliers
hist_1 <- hist(loan_data$loan_amnt)
hist_1$breaks

hist_2 <- hist(loan_data$loan_amnt, breaks = 200, xlab = "Loan amount", main = "Histogram of loan amount")

# Check age for outliers
hist(loan_data$age)
summary(loan_data$age)
boxplot(loan_data$age, horizontal = TRUE)

# Major outlier detected in age
# Find the bad data point
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Income")

# Appears age and income have same bad data in one record
# Remove bad data point by cutting off ages over 100 years
index_high <- which(loan_data$age > 100)
new_data <- loan_data[-index_high, ]
plot(new_data$age, new_data$annual_inc, xlab = "Age", ylab = "Annual Income")

loan_data[index_high, ]

# Check for missing data
summary(new_data)

# NAs prevalent in both int_rate and emp_length categories
hist(new_data$int_rate)
summary(new_data$int_rate)

new_data$int_category <- rep(NA, nrow(new_data))
summary(new_data$int_category)

quantile(new_data$int_rate, seq(0, 1, .2), na.rm = TRUE)

# Replace meaningful NAs by converting continuous data to categorical
# Coarse sort interest rates into quantile buckets and include "Missing" for NAs
new_data$int_category[which(new_data$int_rate <= 7.51)] <- "0.00-7.51"
new_data$int_category[which(new_data$int_rate > 7.51 & new_data$int_rate <= 10.2)] <- "7.51 - 10.20"
new_data$int_category[which(new_data$int_rate > 10.2 & new_data$int_rate <= 11.86)] <- "10.20-11.86"
new_data$int_category[which(new_data$int_rate > 11.86 & new_data$int_rate <= 13.92)] <- "11.86-13.92"
new_data$int_category[which(new_data$int_rate > 13.92 & new_data$int_rate <= 25)] <- "13.92-25.00"
new_data$int_category[which(is.na(new_data$int_rate))] <- "Missing"
new_data$int_category <- as.factor(new_data$int_category)
summary(new_data$int_category)
plot(new_data$int_category)

# Replace NAs in emp_length using coarse classification
summary(new_data$emp_length)

quantile(new_data$emp_length, seq(0, 1, .2), na.rm = TRUE)

new_data$emp_cat <- rep(NA, nrow(new_data))
new_data$emp_cat[which(new_data$emp_length <= 1)] <- "0 - 1"
new_data$emp_cat[which(new_data$emp_length > 1 & new_data$emp_length <= 3)] <- "1 - 3"
new_data$emp_cat[which(new_data$emp_length > 3 & new_data$emp_length <= 5)] <- "3 - 5"
new_data$emp_cat[which(new_data$emp_length > 5 & new_data$emp_length <= 10)] <- "5 - 10"
new_data$emp_cat[which(new_data$emp_length > 10)] <- "10+"
new_data$emp_cat[which(is.na(new_data$emp_length))] <- "Missing"
new_data$emp_cat <- as.factor(new_data$emp_cat)
class(new_data$emp_cat)
summary(new_data$emp_cat)
plot(new_data$emp_cat)

str(new_data)

# Explore new catagorical data
CrossTable(new_data$loan_status, new_data$emp_cat, prop.chisq = F, prop.t = F, prop.r = F)
CrossTable(new_data$loan_status, new_data$int_category, prop.chisq = F, prop.t = F, prop.r = F)

# Remove data columns that have been sorted into factors
new_data$int_rate <- NULL
new_data$emp_length <- NULL

# Randomly sample a training set, and extract the test set
train_ind <- sample(1:nrow(new_data), 2 / 3 * nrow(new_data))
training_set <- new_data[train_ind, ]
test_set <- new_data[-train_ind, ]

# Logistic regression model tests
logistic_model_small <- glm(loan_status ~ emp_cat + int_category, family = binomial, data = training_set)
summary(logistic_model_small)

# Missing employment data and higher interest rates statistically significant
logistic_model_small$coefficients

# Test case
exp(sum(logistic_model_small$coefficients[c(1, 6, 9)])) 
predict(logistic_model_small, newdata = test_set[1, ], type = "response")

predictions_all_small <- predict(logistic_model_small, newdata = test_set, type = "response")
range(predictions_all_small)
hist(predictions_all_small)
boxplot(predictions_all_small, horizontal = TRUE)


# Utilize all data in the training set
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
summary(log_model_full)

predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")
range(predictions_all_full)
summary(predictions_all_full)

boxplot(predictions_all_full, horizontal = TRUE)
hist(predictions_all_full)

# Establish a cutoff for our predictions
cutoff <- quantile(predictions_all_full, .8)
pred_cutoff <- ifelse(predictions_all_full > cutoff, 1, 0)

# Create confusion matrix
conf_matrix <- table(test_set$loan_status, pred_cutoff)
conf_matrix

# Compute classification accuracy
(conf_matrix[1, 1] + conf_matrix[2, 2]) / sum(conf_matrix)

# Compute sensitivity
(conf_matrix[2, 2]) / sum(conf_matrix[2, ])

# Compute specificity
conf_matrix[1, 1] / sum(conf_matrix[1, ])

# Coumpute bad loan rate
accepted_log <- test_set$loan_status[pred_cutoff == 0]
sum(accepted_log) / length(accepted_log)

# Write a function to take default predictions from a logistic regression, set 
# cutoffs at various default rate prediction quantiles over a sequence of acceptince rates,
# and run a for loop over to calculate actual default rate on loans predicted to be good.
# Generate a table of acceptance rates, predicted default cutoff, and actual default rate.

loan_strategy <- function(prob_of_def){
  accept_rate <- seq(1, 0, -0.01)
  cutoff <- rep(NA, length(accept_rate))
  bad_rate <- rep(NA, length(accept_rate))
  for(i in 1:length(accept_rate)){
    cutoff[i] <- quantile(prob_of_def, accept_rate[i])
    pred_i <- ifelse(prob_of_def > cutoff[i], 1, 0)
    pred_as_good <- test_set$loan_status[pred_i == 0]
    bad_rate[i] <- sum(pred_as_good) / length(pred_as_good)
  }
  table <- cbind(accept_rate, cutoff = round(cutoff, 4), bad_rate = round(bad_rate, 4))
  return(list(table = table, bad_rate = bad_rate, accept_rate = accept_rate, cutoff = cutoff))
}

strategy_log_model <- loan_strategy(predictions_all_full)
strategy_log_model$table
strategy_log_model

# Plot bad loan rates against sequence of loan acceptance rates
plot(strategy_log_model$accept_rate, strategy_log_model$bad_rate, 
     type = "l", xlab = "Acceptance Rate", ylab = "Bad Loan Rate", 
     lwd = 2, main = "Default Rates for a Given Loan Acceptance Rate")

# Build a decision tree
tree_model <- rpart(formula = loan_status ~., data = training_set, method = "class")
plot(tree_model, uniform = TRUE)

# Adjust complexity parameter
# Add a loss matrix
tree_model <- rpart(formula = loan_status ~ ., data = training_set, method = "class",
                    control = rpart.control(cp = 0.001), 
                    parms = list(loss = matrix(c(0, 10, 1, 0))))

plot(tree_model, uniform = TRUE)
text(tree_model)

# Check results of model predictions
tree_result <- predict(tree_model, newdata = test_set, type = "response")
range(tree_result[ , 2])

# Establish a cutoff for predicting default
tree_cutoff_15 <- ifelse(tree_result[ , 2] > 0.15, 1, 0)
table(test_set$loan_status, tree_cutoff_15)

# Confusion matrix of test set observations vs. predicted results
tree_cutoff_10 <- ifelse(tree_result[ , 2] > 0.10, 1, 0)
table(test_set$loan_status, tree_cutoff_10)

# Check complexity parameter and cross validation error
printcp(tree_model)
plotcp(tree_model)

# Select cp which results in lowest x-val error
index <- which.min(tree_model$cptable[ , "xerror"])
tree_min <- tree_model$cptable[index, "CP"]

# Prune the tree
pruned_model <- prune(tree_model, cp = tree_min)
plot(pruned_model, uniform = TRUE)
text(pruned_model, use.n = TRUE)

prp(pruned_model)
printcp(pruned_model)

# Test the pruned decision tree model
ptree_result <- predict(pruned_model, newdata = test_set)

# Set a cutoff for a fixed acceptance rate, rejecting 30% of loans
cutoff <- quantile(ptree_result[ , 2], .8)

ptree_result_20 <- ifelse(ptree_result[ , 2] > cutoff, 1, 0)

# Compare to actual results
table(test_set$loan_status, ptree_result_20)
sum(diag(table(test_set$loan_status, ptree_result_20))) / nrow(test_set)

# Check bad rate
accpted_ptree_20 <- test_set$loan_status[ptree_result_20 == 1]
sum(accpted_ptree_20) / length(accpted_ptree_20)                          

# Look at the strategy table and curve for a variety of cutoff points
# for the pruned decision tree model

strategy_tree <- loan_strategy(ptree_result[ , 2])

strategy_tree$table
plot(strategy_tree$accept_rate, strategy_tree$bad_rate, type = "l", lwd = 2, 
     xlab = "Acceptance Rate", ylab = "Bad Loan Rate", 
     main = "Default Rates for a Given Loan Acceptance Rate")

# Plot the receiver operating charactistics ROC
# Then calculate the area under the curve

ROC_log <- roc(test_set$loan_status, predictions_all_full)
ROC_ptree <- roc(test_set$loan_status, ptree_result[ , 2])

plot(ROC_log, col = "blue")
lines(ROC_ptree, col = "red")

auc(ROC_log)
auc(ROC_ptree)
