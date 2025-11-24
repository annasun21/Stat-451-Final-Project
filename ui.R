library(shiny)
library(leaflet)

ui <- navbarPage(
  title = "Global COVID-19 Dashboard",
  
  # ---Panel 1: Monthly trends---
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
        )
      ),
      mainPanel(
        plotOutput(outputId = "plot1")
      )
    )
  ),
  
  # ---Panel 2: Geographic spread---
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
            "New Cases (Rate/person)"  = "cases_rate",
            "New Deaths (Rate/person)" = "deaths_rate"
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
  
  # ---Panel 3: Summary table--
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
        )
      ),
      mainPanel(
        h3("Top 10 countries – summary table"),
        p("Ranked by total COVID-19 cases"),
        tableOutput("geo_table")
      )
    )
  ),
  
  # --- Panel 4: Total COVID Deaths or Cases (Teammate barplot) ---
  
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
  
  # --- Panel 5: COVID Cases vs. Deaths (Teammate scatterplot) ---
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
        )
      ),
      mainPanel(
        plotOutput("covid_plot2")
      )
    )
  )
)
