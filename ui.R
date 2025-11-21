library(shiny)
library(shinydashboard)
library(tidyverse)

covid_data <- read_csv("covid-data.csv")

covid_data <- covid_data %>%
  filter(!is.na(continent))

header <- dashboardHeader(title = "Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Total COVID Deaths or Cases", tabName = "continent"),
    menuItem("COVID Cases vs. Deaths", tabName = "location")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "home",
      fluidRow(
        box(
          width = 12,
          title = "COVID Data Table",
          tableOutput("covid_table")
        )
      )
    ),
    
    tabItem(
      tabName = "continent",
      fluidRow(
        box(
          title = "Controls", width = 4,
          radioButtons("input", "Total Deaths or Cases:", 
                       choices = c("Deaths", "Cases"),
                       selected = "Deaths"),
          selectInput("continent", "Select Continent:",
                      choices = sort(unique(covid_data$continent))),
          sliderInput("n_results", "Number of Countries:", min = 1, max = 55, value = 10),
          radioButtons("order", "Sort Order:", 
                       choices = c("Descending" = "desc", "Ascending" = "asc"),
                       selected = "desc")
        ),
        box(
          title = "Total COVID Deaths or Cases", width = 8,
          plotOutput("covid_plot1")
        )
      )
    ),
    
    tabItem(
      tabName = "location",
      fluidRow(
        box(
          title = "Controls", width = 4,
          selectInput("location", "Select Location:",
                      choices = sort(unique(covid_data$location))),
          dateRangeInput("date", "Select Date Range:",
                         start = min(covid_data$date),
                         end = max(covid_data$date),
                         min = min(covid_data$date),
                         max = max(covid_data$date),
                         format = "yyyy-mm-dd",
                         separator = " to "),
          checkboxInput("trend", "Add Trend Line?", FALSE)
        ),
        box(
          title = "COVID Cases vs. Deaths", width = 8,
          plotOutput("covid_plot2")
        )
      )
    )
  )
)

dashboardPage(header, sidebar, body)
