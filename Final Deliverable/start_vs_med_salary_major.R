library(ggplot2)
library(dplyr)
library(tidyverse)

# Read in csv for major
degrees <- read.csv("degrees-that-pay-back.csv", header = TRUE)

# Clean data
clean_data <- function(col_name) {
  salary <- col_name %>%
    str_replace("\\$", "") %>%
    str_replace(",", "")
  return(salary)
}

# Clean Starting.Median.Salary, Mid.Career.Median.Salary,
# Mid.Career.25th.Percentile.Salary, Mid.Career.75th.Percentile.Salary,
# Mid.Career.90th.Percentile.Salary

start_med_salary <- clean_data(degrees$Starting.Median.Salary)
start_med_salary <- as.numeric(start_med_salary, na.rm = T)

mid_med_salary <- clean_data(degrees$Mid.Career.Median.Salary)
mid_med_salary <- as.numeric(mid_med_salary, na.rm = T)

mid_25_salary <- clean_data(degrees$Mid.Career.25th.Percentile.Salary)
mid_25_salary <- as.numeric(mid_25_salary, na.rm = T)

mid_75_salary <- clean_data(degrees$Mid.Career.75th.Percentile.Salary)
mid_75_salary <- as.numeric(mid_75_salary, na.rm = T)

mid_90_salary <- clean_data(degrees$Mid.Career.90th.Percentile.Salary)
mid_90_salary <- as.numeric(mid_90_salary, na.rm = T)

degrees <- transform(degrees,
  Starting.Median.Salary = start_med_salary,
  Mid.Career.Median.Salary = mid_med_salary,
  Mid.Career.25th.Percentile.Salary = mid_25_salary,
  Mid.Career.75th.Percentile.Salary = mid_75_salary,
  Mid.Career.90th.Percentile.Salary = mid_90_salary
)

# Pivot longer so data can be plotted
salary_data <- degrees %>%
  select(!Mid.Career.10th.Percentile.Salary &
    !Percent.change.from.Starting.to.Mid.Career.Salary) %>%
  pivot_longer(!Undergraduate.Major,
    names_to = "Salary.Type",
    values_to = "Salary"
  )

# Plot scatter plot
major_salary_plot <- ggplot(
  salary_data,
  aes(
    x = reorder(Undergraduate.Major, Salary),
    y = Salary, color = Salary.Type
  )
) +
  geom_point() +
  labs(
    title = "Undergraduate Major vs Salary",
    x = "Undergraduate Major",
    y = "Salary ($)"
  ) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(axis.text.x = element_text(size = 6))
