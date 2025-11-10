# Load libraries
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)

# Load dataset
data("iris")

# Convert to binary classification: setosa vs non_setosa
iris_bin <- iris %>%
  mutate(Species = factor(ifelse(Species == "setosa", "setosa", "non_setosa"),
                          levels = c("non_setosa", "setosa")))

# Train-test split (70-30)
set.seed(123)
idx <- createDataPartition(iris_bin$Species, p = 0.7, list = FALSE)
trainData <- iris_bin[idx, ]
testData <- iris_bin[-idx, ]

# Logistic Regression
log_model <- glm(Species ~ Sepal.Length + Petal.Length,
                 data = trainData, family = binomial())
summary(log_model)

# Predictions
log_prob <- predict(log_model, testData, type = "response")
log_class <- ifelse(log_prob > 0.5, "setosa", "non_setosa")

# Confusion Matrix
confusionMatrix(factor(log_class, levels = levels(testData$Species)),
                testData$Species)

# ROC Curve
roc_obj <- roc(testData$Species, log_prob,
               levels = c("non_setosa", "setosa"))
plot(roc_obj, col = "blue", main = "ROC Curve - Logistic Regression")

# Decision Tree
tree_model <- rpart(Species ~ Sepal.Length + Petal.Length,
                    data = trainData, method = "class")
rpart.plot(tree_model)

# Decision Tree Predictions & Confusion Matrix
tree_pred <- predict(tree_model, testData, type = "class")
confusionMatrix(tree_pred, testData$Species)
