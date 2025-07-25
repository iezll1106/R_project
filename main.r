library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)

# Welcome messages
print("Hello, Welcome to R World!")
cat("Welcome to PGG Employee Salary Analyzer!\n\n")

# Read CSV
employee_data <- read.csv(here("pgg_joemployees.csv"))

# Inspect data
glimpse(employee_data)
print(class(employee_data$designation))
head(employee_data)

# Rename columns: fix dot notation, lowercase, and trim spaces
employee_data <- employee_data %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  rename_with(tolower) %>%
  rename_with(stringr::str_trim)

# Convert total_salary to numeric
employee_data <- employee_data %>%
  mutate(total_salary = as.numeric(gsub(",", "", total_salary)))

# Add salary level
employee_data <- employee_data %>%
  mutate(salary_level = case_when(
    total_salary >= 30000 ~ "High",
    total_salary >= 25000 ~ "Medium",
    TRUE ~ "Low"
  ))

# View preview
head(employee_data[, c("total_salary", "salary_level")])
View(employee_data)

# --- Extended Analysis Below ---

# Total employees by office
offices <- unique(employee_data$office)
cat("\nTotal Employees by Office:\n")
for (office in offices) {
  count <- nrow(filter(employee_data, office == !!office))
  cat(paste(" -", office, ":", count, "employees\n"))
}

# Clean rate_day: remove non-numeric characters (except dot and digits)
employee_data <- employee_data %>%
  mutate(
    rate_day = gsub("[^0-9.]", "", rate_day),
    rate_day = as.numeric(rate_day)
  )

avg_rate_by_office <- employee_data %>%
  group_by(office) %>%
  summarise(average_rate_day = mean(rate_day, na.rm = TRUE))

cat("\n Average Rate per Day by Office:\n")
print(avg_rate_by_office)
View(avg_rate_by_office)

# Distribution by office and gender
gender_dist <- employee_data %>%
  group_by(office, gender) %>%
  summarise(count = n())

cat("\nGender Distribution per Office:\n")
print(gender_dist)
View(gender_dist)

# Total count by gender
gender_totals <- employee_data %>%
  group_by(gender) %>%
  summarise(total = n())

cat("\nTotal Employees by Gender:\n")
print(gender_totals)
View(gender_totals)

# Count of high earners
high_earners <- filter(employee_data, salary_level == "High")
cat("\nNumber of High Earners (≥ 30000):", nrow(high_earners), "\n")

# Compute average salary by designation
avg_salary_by_designation <- employee_data %>%
  group_by(designation) %>%
  summarise(average_salary = mean(total_salary, na.rm = TRUE))

# Plot: Salary Level Distribution
ggplot(employee_data, aes(x = salary_level, fill = salary_level)) +
  geom_bar() +
  labs(
    title = "Employee Salary Levels",
    x = "Salary Level",
    y = "Count"
  ) +
  theme_minimal()

# Plot: Average salary by designation
ggplot(
  avg_salary_by_designation,
  aes(x = reorder(designation, -average_salary), y = average_salary)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Salary by Designation",
    x = "Designation",
    y = "Average Salary"
  ) +
  theme_minimal()

# --- Exporting Summary Reports to CSV ---

write.csv(avg_salary_by_designation, "avg_salary_by_designation.csv", row.names = FALSE) # nolint: line_length_linter.
write.csv(avg_rate_by_office, "avg_rate_by_office.csv", row.names = FALSE)
write.csv(gender_dist, "gender_distribution_by_office.csv", row.names = FALSE)
write.csv(gender_totals, "gender_totals.csv", row.names = FALSE)

cat("\n All summary reports have been saved as CSV files in your project folder.\n") # nolint: line_length_linter.