library(tidyverse)

salaries_by_region <- read.csv("salaries-by-region.csv", header = TRUE)

max_salary_by_region <- salaries_by_region %>%
  group_by(Region) %>%
  summarize(
    Max.Starting.Median.Salary = max(Starting.Median.Salary, na.rm = TRUE),
    Mid.Career.Median.Salary = max(Mid.Career.Median.Salary, na.rm = TRUE),
  )
