# Part E: Correlation & Association
#Datasets: mtcars, iris
  #1. Compute the Pearson correlation between mpg and hp.
  #2. Plot a scatterplot with regression line of mpg ~ hp.
  #3. Create a correlation matrix for all numeric columns in mtcars.
  #4. Compute and interpret the Spearman rank correlation between Sepal.Length and Petal.Length.
  #5. Perform association rule mining (using arules package) on a small market basket dataset.


####-----Answers----####

# Load datasets
data(mtcars)
data(iris)

# 1. Pearson correlation between mpg and hp
cor.test(mtcars$mpg, mtcars$hp, method = "pearson")

# Interpretation:
# H0: No linear relationship between mpg and hp
# If p-value < 0.05 → Significant correlation


# 2. Scatterplot with regression line (mpg ~ hp)
plot(mtcars$hp, mtcars$mpg,
     main = "Scatterplot of mpg vs hp",
     xlab = "Horsepower (hp)",
     ylab = "Miles per Gallon (mpg)",
     pch = 19, col = "blue")
abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2)


# 3. Correlation matrix for all numeric columns in mtcars
cor_matrix <- cor(mtcars)
print(cor_matrix)

# Optional: Visualize correlation matrix
# install.packages("corrplot")  # run once if not installed
library(corrplot)
corrplot(cor_matrix, method = "number")


# 4. Spearman rank correlation between Sepal.Length and Petal.Length
cor.test(iris$Sepal.Length, iris$Petal.Length, method = "spearman")

# Interpretation:
# Spearman correlation checks monotonic (not just linear) relationships.


# 5. Association rule mining on a small market basket dataset
# install.packages("arules")  # run once if not installed
library(arules)

# Example market basket data
transactions <- list(
  c("Milk", "Bread", "Butter"),
  c("Beer", "Bread"),
  c("Milk", "Bread", "Beer", "Butter"),
  c("Milk", "Beer"),
  c("Bread", "Butter")
)

# Convert list to transactions object
trans <- as(transactions, "transactions")

# Mine association rules
rules <- apriori(trans, parameter = list(supp = 0.2, conf = 0.5))

# Show top rules
inspect(rules)
