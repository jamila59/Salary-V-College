library(ggplot2)
library(dplyr)
library(tidyverse)


college_type <- read.csv("salaries-by-college-type.csv", header = TRUE)
start_med_salary <- college_type$Starting.Median.Salary %>%
  str_replace("\\$", "") %>%
  str_replace(",", "")
start_med_salary <- as.numeric(start_med_salary, na.rm = T) / 1000
college_type <- college_type %>%
  mutate("Starting.Median.Thousands" = start_med_salary)
school_salary_plot <- ggplot(data = college_type) +
  geom_boxplot(mapping = aes(x = Starting.Median.Thousands, y = School.Type)) +
  labs(
    title = "School Type versus Starting Median Salary",
    x = "Starting Median Salary ($1K)",
    y = "School Type"
  )
