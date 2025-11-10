# Load required package
library(dplyr)

# Load dataset
data("airquality")
print(head(airquality))   # show first few rows

# 1. Check for missing values
print(sum(is.na(airquality)))          # total NA count
print(colSums(is.na(airquality)))      # missing values per column

# 2. Handle missing values
# Replace missing Ozone values with mean
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)

# Replace missing Solar.R values with median
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)

# Verify dataset after cleaning
print(head(airquality))

# 3. Remove duplicates (example with iris dataset)
data("iris")
iris_with_duplicates <- rbind(iris, iris[1:5, ])      # add duplicates artificially
print(nrow(iris_with_duplicates))                     # before removing duplicates

iris_clean <- distinct(iris_with_duplicates)          # remove duplicates
print(nrow(iris_clean))                               # after removing duplicates

# 4. Change variable formats
iris_clean$Species <- tolower(as.character(iris_clean$Species))
iris_clean$Species <- as.factor(iris_clean$Species)

# View cleaned dataset
print(head(iris_clean))

