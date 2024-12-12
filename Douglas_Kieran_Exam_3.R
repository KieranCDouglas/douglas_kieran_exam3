### Exam 3 
### Kieran Douglas
### 12/11/2024

# prelims 
install.packages("tidyverse")
install.packages("data.table")
library(tidyverse)
library(data.table)


# load the csv file as a data.table
data <- fread("Documents/GitHub/Econometrics2024/ECON 418 518 Exam 3 Data.csv")

# code
indc <- data %>%
  mutate(
    nov_indicator = if_else(time_period == "Nov", 1, 0),
    nj_indicator = if_else(state == 1, 1, 0)
  )

# Calculate mean total employment for each state and time period
mean_employment <- data %>%
  group_by(state, time_period) %>%
  summarize(mean_emp = mean(total_emp, na.rm = TRUE), .groups = "drop")

# Print the results
print(mean_employment)

# Calculate mean employment for each state and time period
mean_employment <- data %>%
  group_by(state, time_period) %>%
  summarize(mean_emp = mean(total_emp, na.rm = TRUE), .groups = "drop")

# Reshape the data to wide format
mean_employment_wide <- mean_employment %>%
  pivot_wider(names_from = time_period, values_from = mean_emp)

# Calculate differences and DiD estimator
did_result <- mean_employment_wide %>%
  mutate(
    difference = Nov - Feb,
    state = if_else(state == 1, "New Jersey", "Pennsylvania")
  ) %>%
  summarize(
    NJ_diff = difference[state == "New Jersey"],
    PA_diff = difference[state == "Pennsylvania"],
    DiD_estimator = NJ_diff - PA_diff
  )

# Print the result
print(did_result)


# running lm
model <- lm(total_emp ~ nj_indicator + nov_indicator + nj_indicator:nov_indicator, data = indc)
summary(model)
# t value
qt(0.975, 764)
# CI
confint(model)["nj_indicator:nov_indicator", ]

# fixed effects model
femodel <- lm(total_emp ~ nj_indicator + nov_indicator + nj_indicator:nov_indicator + factor(restaurant_id) - 1, data = indc)

# Extract and print the coefficients of interest
summary(femodel)$coefficients[c("nj_indicator", "nov_indicator", "nj_indicator:nov_indicator"), ]

