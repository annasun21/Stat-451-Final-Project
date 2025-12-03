library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# Data loading 
df_raw <- read_csv("owid-covid-data.csv", show_col_types = FALSE)

df <- df_raw %>%
  select(
    iso_code,
    continent, location, date,
    new_cases, total_cases,
    new_deaths, total_deaths,
    population, total_vaccinations,
    people_vaccinated
  ) %>%
  mutate(date = as.Date(date))

ui <- navbarPage(
  title = "Global COVID-19 Dashboard",
  
  # Panel 1: Monthly trends
  tabPanel(
    "Monthly trends",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "continents_trend",
          label   = "Select continents:",
          choices = c("Asia", "Europe", "North America",
                      "South America", "Africa", "Oceania"),
          selected = c("Asia", "Europe", "North America",
                       "South America", "Africa", "Oceania")
        ),
        selectInput(
          "metric_trend",
          label = "Select metric:",
          choices = c("New cases"  = "new_cases",
                      "New deaths" = "new_deaths"),
          selected = "new_cases"
        ),
        checkboxInput(
          "log_scale",
          label = "Use log scale?",
          value = TRUE
        ),
        dateRangeInput(
          "trend_dates",
          label = "Date range:",
          start = as.Date("2021-01-01"),
          end   = as.Date("2021-12-31"),
          min   = as.Date("2020-01-01"),
          max   = as.Date("2022-03-05")
        ),
        
        tags$hr(),
        p(
          "Europe, Asia, and North America tend to show some of the highest spikes,",
          "especially in mid-to-late 2021. South America follows similar shapes but",
          "has lower peaks. Africa and Oceania stay much lower overall, though they",
          "still show seasonal increases."
        )
      ),
      mainPanel(
        plotOutput(outputId = "plot1")
      )
    )
  ),
  
  # Panel 2: Geographic spread
  tabPanel(
    "Geographic spread",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "continents_geo",
          label   = "Select continents:",
          choices = c("Asia", "Europe", "North America",
                      "South America", "Africa", "Oceania"),
          selected = c("Asia", "Europe", "North America",
                       "South America", "Africa", "Oceania")
        ),
        radioButtons(
          "geo_metric_type",
          label = "Metric:",
          choices = c(
            "New Cases (Total)"        = "cases_total",
            "New Deaths (Total)"       = "deaths_total",
            "New Cases (per 100k people)"  = "cases_rate",
            "New Deaths (per 100k people)" = "deaths_rate"
          ),
          selected = "cases_total"
        ),
        sliderInput(
          "geo_month",
          label = "Select month:",
          min   = as.Date("2020-01-01"),
          max   = as.Date("2022-03-05"),
          value = as.Date("2021-01-01"),
          step  = 30,
          timeFormat = "%b %Y"
        )
      ),
      mainPanel(
        h3("Geographic spread of COVID-19 new cases/deaths"),
        p("Monthly totals and rates from Jan 2020 to Mar 2022"),
        leafletOutput("geo_map", height = 600)
      )
    )
  ),
  
  # Panel 3: Summary table
  tabPanel(
    "Summary table",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "continents_table",
          label   = "Select continents:",
          choices = c("Asia", "Europe", "North America",
                      "South America", "Africa", "Oceania"),
          selected = c("Asia", "Europe", "North America",
                       "South America", "Africa", "Oceania")
        ),
        sliderInput(
          "table_month",
          label = "Select month:",
          min   = as.Date("2020-01-01"),
          max   = as.Date("2022-03-05"),
          value = as.Date("2021-01-01"),
          step  = 30,
          timeFormat = "%b %Y"
        ),
        
        tags$hr(),
        p(
          "This table lists the top 10 countries with the highest total COVID-19 cases",
          "for the selected month. You can compare how countries differ in new cases,",
          "new deaths, and cumulative totals."
        )
        
      ),
      mainPanel(
        h3("Top 10 countries – summary table"),
        p("Ranked by total COVID-19 cases"),
        tableOutput("geo_table")
      )
    )
  ),
  
  # Panel 4: Total COVID Deaths or Cases
  
  tabPanel(
    "Total deaths/cases by country",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "input",
          "Deaths or Cases:",
          choices  = c("Deaths", "Cases"),
          selected = "Deaths"
        ),
        radioButtons(
          "metric_type",
          "Metric type:",
          choices  = c("Total", "Rate (per person)"),
          selected = "Total"
        ),
        selectInput(
          "continent",
          "Select Continent:",
          choices = sort(unique(df$continent[!is.na(df$continent)]))
        ),
        sliderInput(
          "n_results",
          "Number of Countries:",
          min   = 1,
          max   = 55,
          value = 10
        ),
        radioButtons(
          "order",
          "Sort Order:",
          choices  = c("Descending" = "desc", "Ascending" = "asc"),
          selected = "desc"
        )
      ),
      mainPanel(
        plotOutput("covid_plot1")
      )
    )
  ),
  
  # Panel 5: COVID Cases vs Deaths
  tabPanel(
    "Cases vs. deaths (scatterplot)",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "location",
          "Select Location:",
          choices = sort(unique(df$location))
        ),
        dateRangeInput(
          "date",
          "Select Date Range:",
          start = min(df$date),
          end   = max(df$date),
          min   = min(df$date),
          max   = max(df$date),
          format    = "yyyy-mm-dd",
          separator = " to "
        ),
        checkboxInput(
          "trend",
          "Add Trend Line?",
          FALSE
        ),
        
        tags$hr(),
        p(
          "This plot compares new COVID-19 cases and new deaths for the chosen location.",
          "Each point represents one day. Clusters near the bottom-left show days with low",
          "transmission, while points farther out indicate periods with more severe outbreaks."
        ),
        p(
          "Turning on the trend line helps you see whether deaths rise proportionally with cases",
          "or whether the relationship is weaker at certain times."
        )
      ),
      mainPanel(
        plotOutput("covid_plot2")
      )
    )
  )
)
