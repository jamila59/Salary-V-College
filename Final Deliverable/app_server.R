library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(lintr)
library(styler)
source("start_vs_med_salary_major.R", local = TRUE)
source("table.R", local = TRUE)

# Data cleaning
school <- read.csv("salaries-by-college-type.csv", header = TRUE)
unique_schools <- unique(school$School.Name)
school$starting_median_salary <- as.numeric(
  gsub("[\\$,]", "", school$Starting.Median.Salary)
)
school$mid_career_median_salary <- as.numeric(
  gsub("[\\$,]", "", school$Mid.Career.Median.Salary)
)

salaries_by_region <- read.csv("salaries-by-region.csv", header = TRUE)
salaries_by_region$starting_median_salary <- as.numeric(
  gsub("[\\$,]", "", salaries_by_region$Starting.Median.Salary)
)
salaries_by_region$mid_career_median_salary <- as.numeric(
  gsub("[\\$,]", "", salaries_by_region$Mid.Career.Median.Salary)
)
salaries_by_region$mid_career_90th_percent_salary <- as.numeric(
  gsub("[\\$,]", "", salaries_by_region$Mid.Career.90th.Percentile.Salary)
)
avg_salaries_by_region <- salaries_by_region %>%
  group_by(Region) %>%
  summarize(
    avg_starting_median_salary = mean(starting_median_salary, na.rm = T),
    Avg.Mid.Career.Median.Salary = mean(mid_career_median_salary, na.rm = T),
    Avg.Mid.Career.90th.Percentile.Salary = mean(
      mid_career_90th_percent_salary,
      na.rm = T
    ),
  )
unique_regions <- unique(salaries_by_region$Region)

server <- function(input, output) {

  ## Visualization 1: This chart compares major and salary. The x-axis is major,
  ## and the y-axis is shows the corresponding salary.
  degrees <- read.csv("degrees-that-pay-back.csv", header = TRUE)
  clean_data <- function(col_name) {
    salary <- col_name %>%
      str_replace("\\$", "") %>%
      str_replace(",", "")
    return(salary)
  }

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
  globalVariables(c(
    "Starting.Median.Salary", "Mid.Career.Median.Salary",
    "Mid.Career.25th.Percentile.Salary",
    "Mid.Career.75th.Percentile.Salary",
    "Mid.Career.90th.Percentile.Salary"
  ))

  output$deg_type <- renderPlotly({
    major_salary_plot <- ggplot(
      data = salary_data %>%
        filter(Undergraduate.Major %in% input$major),
      aes(
        x = Undergraduate.Major,
        y = Salary,
        color = Salary.Type
      )
    ) +
      geom_point() +
      scale_y_continuous(
        limits = c(
          min(salary_data$Salary),
          max(salary_data$Salary)
        ),
        labels = scales::dollar_format()
      ) +
      labs(
        title = "Undergraduate Major vs Salary",
        x = "Undergraduate Major",
        y = "Salary ($)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0))
    major_salary_plot
  })














  ## Visualization 2: This chart compares universities and salary. The bar chart
  ## shows the salary for starting median salary and mid-career median salary
  ## for different schools.
  library(data.table)
  output$school_type <- renderPlotly({
    selected_data <- school %>%
      dplyr::filter(School.Name == input$school_name)
    df <- melt(as.data.table(selected_data),
      id.vars = "School.Name",
      measure.vars = c(
        "Starting.Median.Salary",
        "Mid-Career Median Salary" = "Mid.Career.Median.Salary"
      )
    )
    p <- ggplot(data = df, aes(x = variable, y = value)) +
      geom_bar(stat = "identity", fill = input$color) +
      theme_minimal() +
      labs(
        title = "Undergraduate School vs Salary",
        x = "Salary Type",
        y = "Salary ($)"
      ) +
      scale_x_discrete(
        labels = c("Starting Median Salary", "Mid-Career Median Salary")
      )
  })

  ## Visualization 3: This chart compares salary and region of the college.
  ## The bar shows the average starting median salary, average mid-career median
  ## salary, and mid-career 90th percentile salary for different regions.
  output$region <- renderPlotly({
    selected_data <- avg_salaries_by_region %>%
      dplyr::filter(Region == input$region_name)
    df <- melt(
      as.data.table(selected_data),
      id.vars = "Region",
      measure.vars = c(
        "avg_starting_median_salary",
        "Avg. Mid-Career Median Salary" = "Avg.Mid.Career.Median.Salary",
        "Avg. Mid-Career 90th Percentile Salary" =
          "Avg.Mid.Career.90th.Percentile.Salary"
      )
    )
    p <- ggplot(data = df, aes(x = variable, y = value)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme_minimal() +
      labs(
        title = "Undergraduate School vs Salary",
        x = "Salary Type",
        y = "Salary ($)"
      ) +
      scale_x_discrete(
        labels = c(
          "Starting Median Salary",
          "Mid-Career Median Salary",
          "Mid-Career 90th Percentile Salary"
        )
      )
  })

  # Summary
  output$static_major_salary <- renderPlot(major_salary_plot)
  output$region_salary <- renderTable(max_salary_by_region)
}
