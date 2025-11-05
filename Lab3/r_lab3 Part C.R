#Part C: Estimation & Confidence Intervals

#Dataset: mtcars
#1. Construct a 95% confidence interval for the mean of mpg.
#2. Use bootstrapping (boot package) to estimate CI for hp (horsepower).
#3. Compare confidence intervals of mpg for automatic vs manual cars (am variable).


###-------Answers--------####

# Load dataset
data(mtcars)

# 1. 95% Confidence Interval for mean of mpg
t.test(mtcars$mpg, conf.level = 0.95)

# 2. Bootstrapping CI for hp (horsepower)  
library(boot)

# Define a function to calculate the mean
mean_fun <- function(data, indices) {
  mean(data[indices])
}

# Perform bootstrapping (1000 resamples)
set.seed(123)
boot_hp <- boot(data = mtcars$hp, statistic = mean_fun, R = 1000)

# Bootstrap 95% confidence interval
boot.ci(boot_hp, type = "perc")

# 3. Compare CI of mpg for automatic vs manual cars
# am = 0 (automatic), 1 (manual)
auto_mpg <- mtcars$mpg[mtcars$am == 0]
manual_mpg <- mtcars$mpg[mtcars$am == 1]

t.test(auto_mpg, manual_mpg, conf.level = 0.95)

#printing results
cat("95% CI for mean mpg (all cars):\n")
print(t.test(mtcars$mpg)$conf.int)

cat("\nBootstrap 95% CI for mean hp:\n")
print(boot.ci(boot_hp, type = "perc"))

cat("\n95% CI for mpg by transmission type:\n")
print(t.test(auto_mpg, manual_mpg)$conf.int)
