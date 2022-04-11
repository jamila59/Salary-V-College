library("shiny")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(shinyWidgets)

introduction <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
  h3("Data Analysis of Salary and Undergraduate Degree"),
  sidebarLayout(
    sidebarPanel(
      img(alt = "Salary", src = "image.jpeg")
    ),
    mainPanel(
   p("As college students, understanding the value of our degrees and how we can
    maximize salary after graduation helps us guide our decisions. Therefore,our
    groups' area of interest lies in interpreting how different factors such
    as type of college, region of college and type of majors earned impact the
    overall salary of the graduate over time. In our analysis we will be
    examining three datasets which hold observations relating to: (1) choice of
    major (2) college type, and (3) region of attendance. Inspecting these
    datasets will allows our group to answer questions relating to how these
    factor interplay with one another such as (1) How do different undergraduate
    major differ from each other in relation to salary? (2) Which majors see the
    greatest growth? Which majors see the smallest growth? (3) Which region's
    school has low/high salary? The datset we used comes from Kaggle."),
      a("Click here to find data source",
        href = "https://www.kaggle.com/wsj/college-salaries"
      ),
   absolutePanel(HTML("Project By: <br/> Jamila Aliveya<br/> Molly Wu<br/>
                      Tanner Peterson<br/> Sophia Hwang<br/>
                      <b>INFO 201 | WIN2021</b>"),
                 bottom = 0, right = 0, fixed = TRUE,
                 style = "background-color: white;
                         color: #86898D;
                         font-size: 12px;
                         opacity: 0.85;
                         padding: 20px 20px 20px 20px;
                         margin: auto;
                         text-align: right;
                         padding-bottom: 2mm;
                         padding-top: 1mm;")
    )
  )
)

summary <- tabPanel(
  "Summary",
  titlePanel("Summary"),
  p("Based on our analysis, we observed three major takeaways:"),
  h3("1. Major in Chemical Engineering for the Highest Median Salary"),
  p("Students who graduate from Engineering tend to have higher salaries for all
  types of salaries (starting, median, mid career, etc) as shown in the graph
  below, and out of the engineering majors Chemical Engineering is the most
  lucrative. Physician Assistants have the highest starting salary, followed by
  Chemical and Computer Engineering. For mid career median salary, Chemical,
  Electrical and Computer Engineering are the top three. In the 10th percentile
  of mid career salaries, the top three were Chemical Engineering, Electrical
  Engineering, and Physician Assistant. In the 90th percentile the top three
  were Economics, Finance, and Chemical Engineering. Therefore, those who want
  to maximize their salary regardless of where in their career they are might
  look to Chemical Engineering."),

  h3("2. Nursing Majors Experience Little Growth in Career Salary"),
  p("Another interesting observation to note is that Nursing major salaries are
    tightly bound and tend to see very little growth in their careers, which is
    surprising considering their critical roles in healthcare. They have only
    23.6% growth, second lowest only to Physician Assistant. However, Nursing
    majors also make $20,100 lower than PAs for their starting median salary,
    despite being in the upper half of all majors for starting salary. In
    general, if you prioritize career growth over salary, Nursing would not be
    a good fit for you."),

  plotOutput(outputId = "static_major_salary"),

  h3("3. California is the Most Lucrative Region"),
  p("Out of all the regions categorized in the data table below, California has
    the highest salary in the United States for both starting and mid career
    median salaries. Though the range of starting salaries varies widely by
    region, they are much closer for mid career salaries with a range of about
    $2000. The fact that California is on top is not surprising, as it is home
    to Silicon Valley and a giant tech industry. It is no surprise then as well
    that the highest salary school is located in CA
    (California Institute of Technology). For the greatest salary potential,
    California would be the best place to live."),
  tableOutput(outputId = "region_salary")
)

salary_degree <- tabPanel(
  "Salary vs. Degree",
  titlePanel("What Degree is Worth the Money?"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "major",
        label = "Select Major",
        choices = c(
          "Accounting", "Anthropology", "Agriculture", "Chemistry", "Film",
          "Aerospace Engineering", "Art History", "Biology", "Construction",
          "Business Management", "Chemical Engineering", "Civil Engineering",
          "Communications", "Computer Engineering", "Computer Science",
          "Music", "Criminal Justice", "Drama", "Economics", "Education",
          "Electrical Engineering", "English", "Finance", "Forestry",
          "Geography", "Graphic Design", "Health Care Administration",
          "History", "Hospitality & Tourism", "Industrial Engineering",
          "Information Technology (IT)", "Interior Design", "Journalism",
          "International Relations", "Management Information Systems",
          "Marketing", "Math", "Mechanical Engineering", "Nursing",
          "Nutrition", "Philosophy", "Physician Assistant", "Physics",
          "Political Science", "Psychology", "Religion", "Sociology"
        ),
        selected = "Chemistry",
        options = list(
          # `actions-box` = TRUE,
          `none-selected-text` = "Please make a selection!",
          "min-options" = 1,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    ),

    ### Main panel displays the lineplot
    mainPanel(
      h3("Salary vs. Major Comparison"),
      plotlyOutput(outputId = "deg_type"),
      p("The chart above reveals ranges of salary and salary growth observed by
        various undergraduate majors. The user is able to manipulate this chart
        to display different selections of degrees which can be used to compare
        how degrees grow in salary. The user can see the starting salary,
        mid-career median salary, mid career 25th, 75th, and 90th percentile
        salary for the specific major they chose. This chart also offers an
        interactivity of hovering over points to view their overview
        information.")
    )
  )
)

school_salary <- tabPanel(
  "Salary vs. School",
  titlePanel("What Schools Worth the Money?"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "school_name",
        label = "School Name",
        choices = unique_schools
      ),

      selectInput(
        inputId = "color",
        label = "Bar Color",
        choices = c("blue", "red", "purple", "yellow", "grey")
      )
    ),
    mainPanel(
      h3("School Type vs. Salary Bar Chart"),
      plotlyOutput(outputId = "school_type"),
      p("This bar chart reveals the median starting salary and median mid-career
        salary across many different colleges (inlcuding both public and private
        college). The users can manipulate this chart to display which college
        they want to look into. After selecting the college, the user can
        also select displaying color for aethetic purposes. This chart provides
        a great overview for people who are insterested in discoverng going to
        which college can result in higher starting salary and which college
        prepare students better for their mid-career.")
    )
  )
)

region_salary <- tabPanel(
  "Salary vs. Region",
  titlePanel("Where Should You Attend School?"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "region_name",
        label = "Region",
        choices = unique_regions
      ),
    ),
    mainPanel(
      h3("Salary vs. Region Bar Chart"),
      plotlyOutput(outputId = "region"),
      p("This bar chart illustrates the average starting salary, average median
      mid-career, and average mid-carrer 90th percentile salary of across 5
      different U.S. regions. The users can manipulate this chart to display
      which region they want to look into. The regions selection includes
      California, Western, Midwestern, Southern, and Northeastern. This bar
      chart helps the user to understand which region's colleges will result in
      better salary after graduation.")
    )
  )
)

ui <- navbarPage(
  "College Salary Exploration",
  introduction,
  salary_degree,
  school_salary,
  region_salary,
  summary,
  theme = "styles.css"
)
