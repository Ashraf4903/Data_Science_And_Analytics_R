#Part D: Hypothesis Testing

#Datasets: iris, Titanic (CSV)
  #1. Perform a one-sample t-test: Is the mean Sepal.Length significantly different from 5.5?
  #2. Perform a two-sample t-test: Is there a significant difference in mpg between automatic and manual cars?
  #3. Conduct a chi-square test of independence: Are Survived and Sex independent in the Titanic dataset?
  #4. Perform a one-way ANOVA: Compare Sepal.Length means across the 3 iris species.
  #5. Apply a post-hoc Tukey HSD test after ANOVA and interpret.

# Part D: Hypothesis Testing

# 1. One-sample t-test: Is mean Sepal.Length different from 5.5?
data(iris)
t.test(iris$Sepal.Length, mu = 5.5)

# Interpretation:
# H0: Mean Sepal.Length = 5.5
# H1: Mean Sepal.Length ≠ 5.5
# If p-value < 0.05 → Reject H0 (mean is significantly different from 5.5)


# 2. Two-sample t-test: mpg between automatic and manual cars
data(mtcars)
t.test(mpg ~ am, data = mtcars, var.equal = TRUE)

# Interpretation:
# H0: Mean mpg(auto) = Mean mpg(manual)
# H1: They are different.
# Check p-value to conclude.


# 3. Chi-square test: Are Survived and Sex independent in Titanic dataset?
# (Make sure titanic.csv is in your working directory)
titanic <- read.csv("Titanic.csv")

# Convert to table form
tbl <- table(titanic$Survived, titanic$Sex)
chisq.test(tbl)

# Interpretation:
# H0: Survived and Sex are independent
# H1: They are not independent.
# If p-value < 0.05 → variables are associated.


# 4. One-way ANOVA: Compare Sepal.Length means across 3 iris species
anova_model <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_model)

# Interpretation:
# H0: All species have equal mean Sepal.Length
# H1: At least one mean is different.


# 5. Post-hoc Tukey HSD test after ANOVA
TukeyHSD(anova_model)

# Interpretation:
# Shows pairwise differences between species.
# Look for pairs with p < 0.05 → significantly different means.
