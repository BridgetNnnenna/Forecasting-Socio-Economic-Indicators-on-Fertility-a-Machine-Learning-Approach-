rm(list=ls())
library(MASS)
data("swiss")
swiss<-na.omit(swiss)
names(swiss)
##Fit a Simple linear regression 
Fertility <-swiss$Fertility
Education <-swiss$Education
Infant.Mortality<-swiss$Infant.Mortality
Agriculture<-swiss$Agriculture
Catholic<-swiss$Catholic
lm.fit_A <-lm(swiss$Fertility~swiss$Agriculture)
summary(lm.fit_A)
lm.fit_E <-lm(swiss$Fertility~swiss$Education)
summary(lm.fit_E)
lm.fit_I <-lm(swiss$Fertility~swiss$Infant.Mortality)
summary(lm.fit_I)
lm.fit_C <-lm(swiss$Fertility~swiss$Catholic)
summary(lm.fit_C)

#Regression Lines 
par(mfrow=c(2,2))
plot(swiss$Fertility~swiss$Agriculture, main = "Fertility vs Agriculture", xlab = "Agriculture", ylab = "Fertility")
abline(coef=coef(lm.fit_A), col = "Red")

plot(swiss$Fertility~swiss$Education, main = "Fertility vs Education", xlab = "Agriculture", ylab = "Education")
abline(coef=coef(lm.fit_E), col = 2)

plot(swiss$Fertility~swiss$Infant.Mortality, main = "Fertility vs Infant.Mortality", xlab = "Infant.Mortality", ylab = "Fertility")
abline(coef=coef(lm.fit_I), col = 3)

plot(swiss$Fertility~swiss$Catholic, main = "Fertility vs Catholic", xlab = "Catholic", ylab = "Fertility")
abline(coef=coef(lm.fit_C), col = "Blue")

#Multiple linear regression 
Multilm.fit<-lm(Fertility~Agriculture + Catholic + Education + Infant.Mortality, data = swiss)
summary(Multilm.fit)

##Classification approach 
mean_fertility <- mean(swiss$Fertility)
swiss$Fertility_Class <- ifelse(swiss$Fertility > 70, 1, 0)
head(swiss)
swiss$Fertility_class<-ifelse(swiss$Fertility>70,1,0)

#Logistic Regression 
library(stats)
set.seed(1) #for reproduction 
train_index<-sample(1:nrow(swiss), round(0.7*nrow(swiss)))
train_data<- swiss[train_index,]
test_data<-swiss[-train_index,]
glm_model<-glm(Fertility_class ~. - Fertility - Examination, data = train_data, family = binomial)
summary(glm_model)

logistic_predictions<-predict(glm_model, newdata = test_data, type = "response")
logistic_class<-ifelse(logistic_predictions>0.5, 1, 0)
confusion_matrix<-table(logistic_class, test_data$Fertility_class)
print(confusion_matrix)
misclassification_rate<-mean(logistic_class != test_data$Fertility_class); misclassification_rate
accuracy <- mean(logistic_class == test_data$Fertility_class); accuracy


## Linear Discriminant Analysis 
lda_model <-lda(Fertility_class ~. - Fertility - Examination, data = train_data)
summary(lda_model)
lda_model
lda_predictions <- predict(lda_model, newdata = test_data)
confusion_matrix <- table(lda_predictions$class, test_data$Fertility_class)
print(confusion_matrix)
lda_predictions <- predict(lda_model, newdata = test_data)$class
misclassification_rate <- mean(lda_predictions != test_data$Fertility_class);misclassification_rate
accuracy <- mean(lda_predictions == test_data$Fertility_class); accuracy


## Quadratic Discriminant Analysis 
set.seed(1)
qda_model<- qda(Fertility_class ~. - Fertility - Examination, data = train_data)
qda_model
qda_predictions <- predict(qda_model, newdata = test_data)$class
confusion_matrix<-table(qda_predictions, test_data$Fertility_class)
misclassification_rate<-mean(qda_predictions != test_data$Fertility_class); misclassification_rate
accuracy<-mean(qda_predictions == test_data$Fertility_class); accuracy

##KNN 
predictors<-swiss[,c("Agriculture", "Education", "Catholic", "Infant.Mortality")]
response<-swiss$Fertility_class
predictors <- scale(predictors)
library(caTools)
set.seed(1)
split <- sample.split(response, SplitRatio = 0.7)
train_data <- predictors[split, ]
train_labels <- response[split]
test_data <- predictors[!split, ]
test_labels <- response[!split]
library(class)

k_values <- seq(1, 15, by = 2)
for (k in k_values) {
knn_model <- knn(train_data, test_data, train_labels, k = k)
accuracy <- sum(knn_model == test_labels) / length(test_labels)
cat("KNN (k =", k, ") Accuracy:", accuracy, "\n")
}
length(train_labels) == nrow(train_data)  # Should return TRUE
length(test_labels) == nrow(test_data)

###Support vector machine
library(e1071)
set.seed(1)
split <- sample.split(response, SplitRatio = 0.7)
train_data <- data.frame(predictors[split, ], Fertility_class = response[split])
test_data <- data.frame(predictors[!split, ], Fertility_class = response[!split])
svm.mod = svm ( Fertility_class ~ Agriculture + Catholic + Education + Infant.Mortality, data = swiss, type = "C-classification", kernel = "linear")
svm_predictions <- predict(svm.mod, newdata = test_data)
accuracy <- mean(svm_predictions == test_data$Fertility_class);accuracy
misclassification_rate <- mean(svm_predictions != test_data$Fertility_class);misclassification_rate

## Lasso Regression 
predictors <-swiss[,c("Agriculture", "Education", "Catholic", "Infant.Mortality")]
response<-swiss$Fertility
library(glmnet)
cv.fold_lasso<-cv.glmnet(as.matrix(predictors), response, alpha = 1)
plot(cv.fold_lasso)
lambda_lasso<-cv.fold_lasso$lambda.min;lambda_lasso
lambda_lasso_1se <- cv.fold_lasso$lambda.1se
lasso_model<-glmnet(as.matrix(predictors), response, alpha = 1, lamda = lamda_lasso)
coefficients_min <- predict(cv.fold_lasso, s = lambda_lasso, type = "coefficients")
print(coefficients_min)
coefficients_1se <- predict(cv.fold_lasso, s = lambda_lasso_1se, type = "coefficients")
print(coefficients_1se)


## Ridge regression 
cv.fold_ridge <- cv.glmnet(as.matrix(predictors), response, alpha = 0)
plot(cv.fold_ridge)
lambda_ridge <- cv.fold_ridge$lambda.min; lambda_ridge

ridge_model <- glmnet(as.matrix(predictors), response, alpha = 0, lambda = lambda_ridge)
lasso_predictions <- predict(lasso_model, as.matrix(predictors))
ridge_predictions <- predict(ridge_model, as.matrix(predictors))
install.packages("Metrics")
library(Metrics)
lasso_rmse <- rmse(lasso_predictions, response)
ridge_rmse <- rmse(ridge_predictions, response)
cat("Lasso RMSE:", lasso_rmse, "\n")
cat("Ridge RMSE:", ridge_rmse, "\n")

lambda_ridge_1se <- cv.fold_ridge$lambda.1se
coefficients_min <- predict(cv.fold_ridge, s = lambda_ridge, type = "coefficients")
print(coefficients_min)
coefficients_1se <- predict(cv.fold_ridge, s = lambda_ridge_1se, type = "coefficients")
print(coefficients_1se)


#comparing with linear regression
predictors <- swiss[,c("Agriculture", "Education", "Catholic", "Infant.Mortality")]
response <- swiss$Fertility
linear_model<-lm(response~Agriculture + Catholic + Education + Infant.Mortality, data = swiss)
linear_predictions <- predict(Multilm.fit, newdata = swiss)
rmse_linear <- rmse(linear_predictions, response)
mse_linear <- mse(linear_predictions, response)
mse_lasso <- mse(lasso_predictions, response)
mse_ridge <- mse(ridge_predictions, response)
mae_linear <- mae(linear_predictions, response)
mae_lasso <- mae(lasso_predictions, response)
mae_ridge <- mae(ridge_predictions, response)
cat("Linear Regression:\n")
cat("MSE:", mse_linear, "\n")
cat("RMSE:", rmse_linear, "\n")
cat("MAE:", mae_linear, "\n")
cat("Lasso Regression:\n")
cat("MSE:", mse_lasso, "\n")
cat("RMSE:", lasso_rmse, "\n")
cat("MAE:", mae_lasso, "\n")
cat("Ridge Regression:\n")
cat("MSE:", mse_ridge, "\n")
cat("RMSE:", ridge_rmse, "\n")
cat("MAE:", mae_ridge, "\n")
