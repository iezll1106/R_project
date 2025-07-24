# Load necessary libraries
library(tidyverse)
library(dplyr)

# Print welcome message
print("Hello, Welcome to R World!")

# Load 'here' package to use relative paths
library(here)

# Read CSV using a relative path
employee_data <- read.csv(
  here("pgg_joemployees.csv")
)

# Show structure and class
glimpse(employee_data)

# Ensure column name matches actual header
print(class(employee_data$designation))

# Print first 5 rows
head(employee_data)

# Rename columns: convert dot notation to snake_case and lowercase
employee_data <- employee_data %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  rename_with(tolower)

# Clean Total_Salary (remove commas and convert to numeric)
employee_data <- employee_data %>%
  mutate(total_salary = as.numeric(gsub(",", "", total_salary)))

# Add salary level column using case_when
employee_data <- employee_data %>%
  mutate(salary_level = case_when(
    total_salary >= 30000 ~ "High",
    total_salary >= 25000 ~ "Medium",
    TRUE ~ "Low"
  ))

# Show new columns
head(employee_data[, c("total_salary", "salary_level")])

# Add this line to open the full dataset in a spreadsheet viewer
View(employee_data)

"Hello World!"
5
10
15 + 20
3111 * 540

name <- "John Doe"
age <- 30
print(paste("Name:", name))
print(paste("Age:", age))

x <- c(1, 2, 3, 4, 5)
y <- c(6, 7, 8, 9, 10)
plot(x, y, main="Simple Plot", xlab="X-axis", ylab="Y-axis", col="blue", pch=19) # nolint
summary(x)
summary(y)