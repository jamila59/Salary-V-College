library(ggplot2)
library(dplyr)
library(tidyverse)

# Read in csv for college type and region
college_med_salary <- read.csv("salaries-by-college-type.csv", header = TRUE)
college_region <- read.csv("salaries-by-region.csv", header = TRUE) %>%
  select(School.Name, Region)

# Clean data to read salaries as numeric and in thousands
start_salary_thousand <- college_med_salary$Starting.Median.Salary %>%
  str_replace("\\$", "") %>%
  str_replace(",", "")
start_salary_thousand <- as.numeric(start_salary_thousand, na.rm = T) / 1000
mid_med_salary_thousand <- college_med_salary$Mid.Career.Median.Salary %>%
  str_replace("\\$", "") %>%
  str_replace(",", "")
mid_med_salary_thousand <- as.numeric(mid_med_salary_thousand, na.rm = T) / 1000

# Calculate percent growth
college_med_salary <- college_med_salary %>%
  mutate(
    "Mid.Career.Percent.Growth" =
      100 * (mid_med_salary_thousand - start_salary_thousand) /
        start_salary_thousand
  )

# Join tables and filter to get liberal arts schools and their regions
liberal_arts <- college_med_salary %>%
  filter(School.Type == "Liberal Arts") %>%
  left_join(college_region, by = "School.Name")

# Plot bar graph School Name vs Percent Growth in Salary,
# colored by school region
liberal_arts_plot <- ggplot(liberal_arts, aes(
  fill = Region, y = Mid.Career.Percent.Growth,
  x = reorder(School.Name, Mid.Career.Percent.Growth)
)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    title = "Salary Growth for Liberal Arts Schools",
    x = "School Name",
    y = "Percent Growth from Starting to Mid Career Median Salary"
  ) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 6))
