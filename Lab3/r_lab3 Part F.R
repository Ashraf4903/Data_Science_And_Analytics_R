#Part F: Mini Data Science Applications

#1. Using Titanic dataset:
  # - Build a logistic regression model predicting Survived using Age, Sex, and Pclass.
  #- Interpret coefficients and odds ratios.
#2. Using iris dataset:
  # - Fit a linear regression model predicting Petal.Length from Sepal.Length.
  #- Plot fitted line + residuals.
#3. Using mtcars dataset:
  # - Cluster cars using k-means clustering (k = 3) on mpg, hp, wt.
  #- Visualize clusters.


####------Answers-----######

#

# --- 1. 

# Load Titanic data (make sure "Titanic.csv" is in your working directory)
titanic <- read.csv("Titanic.csv")

# Convert categorical variables
titanic$Sex <- factor(titanic$Sex)
titanic$Pclass <- factor(titanic$Pclass)

# Build logistic regression model
model_titanic <- glm(Survived ~ Age + Sex + Pclass,
                     data = titanic, family = binomial)

# Summary of model
summary(model_titanic)

# Odds ratios
exp(coef(model_titanic))

# Interpretation:
#  - If OR > 1 → variable increases survival odds
#  - If OR < 1 → variable decreases survival odds
#  - p-value < 0.05 → significant effect


# --- 2. 

data(iris)

# Fit linear regression model
model_iris <- lm(Petal.Length ~ Sepal.Length, data = iris)
summary(model_iris)

# Plot fitted line
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Petal.Length vs Sepal.Length",
     xlab = "Sepal Length", ylab = "Petal Length",
     pch = 19, col = "blue")
abline(model_iris, col = "red", lwd = 2)

# Plot residuals
plot(model_iris$fitted.values, resid(model_iris),
     main = "Residual Plot",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, lty = 2, col = "red")

# Interpretation:
#  - Positive slope → longer sepals = longer petals
#  - Check residuals for randomness (good model fit)


# --- 3. 

data(mtcars)

# Select variables
data_cluster <- mtcars[, c("mpg", "hp", "wt")]

# Scale the data (important for clustering)
data_scaled <- scale(data_cluster)

# Apply K-means (k = 3)
set.seed(123)
km_model <- kmeans(data_scaled, centers = 3, nstart = 20)

# Add cluster info to data
mtcars$cluster <- as.factor(km_model$cluster)

# View cluster summary
table(mtcars$cluster)

# Visualize clusters
# install.packages("ggplot2")  # run once if not installed
library(ggplot2)

ggplot(mtcars, aes(x = hp, y = mpg, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering (k = 3)",
       x = "Horsepower", y = "Miles per Gallon") +
  theme_minimal()
