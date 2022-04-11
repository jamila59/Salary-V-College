library(tidyverse)

major <- read.csv("degrees-that-pay-back.csv", header = TRUE)
region <- read.csv("salaries-by-region.csv", header = TRUE)
college <- read.csv("salaries-by-college-type.csv", header = TRUE)

summary_info <- list()
summary_info$num_observations_majors <- nrow(major)
summary_info$num_observation_colleges <- nrow(college)
summary_info$num_observation_region <- nrow(region)
summary_info$max_salary_major <- major %>%
  filter(Starting.Median.Salary == max(Starting.Median.Salary, na.rm = T)) %>%
  pull(Undergraduate.Major)
summary_info$highet_starting_salary_region <- region %>%
  filter(Starting.Median.Salary == max(Starting.Median.Salary, na.rm = T)) %>%
  pull(Region)
summary_info$highest_starting_salary_school <- college %>%
  filter(Starting.Median.Salary == max(Starting.Median.Salary, na.rm = T)) %>%
  pull(School.Name)
