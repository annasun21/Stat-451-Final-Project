library(shiny)
library(shinydashboard)
library(tidyverse)

function(input, output) {
  
  covid_data <- read_csv("covid-data.csv")
  
  covid_data <- covid_data %>%
    filter(!is.na(continent))
  
  output$covid_table <- renderTable({
    head(covid_data)
  })
  
  covid_continent <- reactive({
    data <- covid_data %>%
      filter(continent == input$continent) %>%
      mutate(total_deaths = if_else(is.na(total_deaths), 0, total_deaths),
             total_cases = if_else(is.na(total_cases), 0, total_cases)) %>%
      group_by(location) %>%
      summarise(total_deaths = max(total_deaths, na.rm = TRUE),
                total_cases = max(total_cases,na.rm = TRUE))
    
    if (input$order == "desc") {
      data <- data %>%
        arrange(desc(total_deaths))
    } else {
      data <- data %>%
        arrange(total_deaths)
    }
    
    data <- data %>% 
      head(input$n_results)
  })
  
  output$covid_plot1 <- renderPlot({
    if (input$input == "Deaths") {
      covid_continent() %>%
        ggplot(aes(x = reorder(location, total_deaths), y = total_deaths)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Total COVID-19 Deaths by Country",
             x = "Country", y = "Deaths") +
        theme_minimal()
    } else {
      covid_continent() %>%
        ggplot(aes(x = reorder(location, total_cases), y = total_cases)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Total COVID-19 Cases by Country",
             x = "Country", y = "Cases") +
        theme_minimal()
    }
  })
  
  
  covid_location <- reactive({
    data <- covid_data %>%
      select(location, new_cases, new_deaths, date) %>%
      filter(location == input$location, 
             date >= input$date[1],
             date <= input$date[2]) %>%
      mutate(new_cases = if_else(is.na(new_cases), 0, new_cases),
             new_deaths = if_else(is.na(new_deaths), 0, new_deaths))
  })
  
  output$covid_plot2 <- renderPlot({
    plot <- covid_location() %>%
      ggplot(aes(x = new_cases, y = new_deaths)) +
      geom_point(alpha = 0.5) +
      labs(
        title = str_wrap(
          paste("Relationship Between New COVID-19 Cases and New Deaths in Afghanistann"),
          width = 50
        ),
        x = "New Cases per Day",
        y = "New Deaths per Day",
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text = element_text(size = 11)
      )
    
    if(input$trend){ 
      plot = plot + geom_smooth(method = "lm") 
    } 
    
    print(plot)
  })
}