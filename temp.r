# Load necessary libraries
library(tidyverse)
library(dplyr)

# Print welcome message
print("Hello, welcome to R World!")

# Read CSV into consistent variable name
employee_data <- read.csv(
  "C:/Users/P C/Documents/GitHub/R_project/pgg_joemployees.csv"
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