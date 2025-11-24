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

# World polygons for leaflet
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


server <- function(input, output) {
  
  # Data for teammate panels (Panels 4 & 5) 
  covid_data <- df %>%
    filter(!is.na(continent))
  
  # PANEL 1:MONTHLY TRENDS (LINE CHART)
  
  monthly_data <- reactive({
    metric <- input$metric_trend
    
    df %>%
      mutate(month = floor_date(date, "month")) %>%
      filter(
        date >= input$trend_dates[1],
        date <= input$trend_dates[2],
        !is.na(continent), continent != "",
        continent %in% input$continents_trend
      ) %>%
      group_by(continent, month) %>%
      summarise(
        value = sum(.data[[metric]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        continent = factor(
          continent,
          levels = c("Asia", "Europe", "North America",
                     "South America", "Africa", "Oceania")
        )
      )
  })
  
  
  output$plot1 <- renderPlot({
    dat <- monthly_data()
    
    y_lab <- if (input$metric_trend == "new_cases") "New cases" else "New deaths"
    
    title_lab <- paste(
      "Monthly",
      ifelse(input$metric_trend == "new_cases",
             "New COVID-19 Cases", "New COVID-19 Deaths"),
      "Across Continents"
    )
    
    subtitle_lab <- paste(
      "Aggregated monthly totals from",
      format(input$trend_dates[1], "%b %Y"), "to",
      format(input$trend_dates[2], "%b %Y")
    )
    
    p <- ggplot(dat, aes(x = month, y = value,
                         color = continent, group = continent)) +
      geom_line(linewidth = 1) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(
        title    = title_lab,
        subtitle = subtitle_lab,
        x        = "Month",
        y        = y_lab,
        color    = "Continent"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title  = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    if (isTRUE(input$log_scale)) {
      p <- p +
        scale_y_log10(
          labels = label_number(scale_cut = cut_short_scale()),
          breaks = c(1e3, 1e4, 1e5, 1e6, 1e7),  
          minor_breaks = NULL                  
        ) +
        theme(panel.grid.minor.y = element_blank())  
    } else {
      p <- p +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
    }
    
    p
  })
  
  
  # PANEL 2: GEOMAP (LEAFLET)
  
  # Monthly aggregated data per country (cases, deaths, population, rates)
  geo_monthly_data <- reactive({
    req(input$geo_month)
    
    selected_month <- floor_date(input$geo_month, "month")
    
    df %>%
      mutate(month = floor_date(date, "month")) %>%
      filter(
        month == selected_month,
        !is.na(continent), continent != "",
        continent %in% input$continents_geo
      ) %>%
      group_by(iso_code, location, continent, month) %>%
      summarise(
        new_cases  = sum(new_cases,  na.rm = TRUE),
        new_deaths = sum(new_deaths, na.rm = TRUE),
        population = max(population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        cases_rate  = if_else(population > 0, new_cases  / population, NA_real_),
        deaths_rate = if_else(population > 0, new_deaths / population, NA_real_)
      )
  })
  
  
  # Join with world polygons and pick metric based on user input
  geo_map_data <- reactive({
    dat <- geo_monthly_data()
    req(nrow(dat) > 0)
    
    metric_choice <- input$geo_metric_type
    
    dat <- dat %>%
      mutate(
        metric_value = dplyr::case_when(
          metric_choice == "cases_total"  ~ new_cases,
          metric_choice == "deaths_total" ~ new_deaths,
          metric_choice == "cases_rate"   ~ cases_rate,
          metric_choice == "deaths_rate"  ~ deaths_rate
        )
      )
    
    world_sf %>%
      left_join(dat, by = c("iso_a3" = "iso_code"))
  })
  
  
  # Render Leaflet map
  output$geo_map <- renderLeaflet({
    map_dat <- geo_map_data()
    
    validate(
      need(any(!is.na(map_dat$metric_value)),
           "No data available for this selection.")
    )
    
    metric_label <- dplyr::case_when(
      input$geo_metric_type == "cases_total"  ~ "New Cases (Total)",
      input$geo_metric_type == "deaths_total" ~ "New Deaths (Total)",
      input$geo_metric_type == "cases_rate"   ~ "New Cases (Rate per person)",
      input$geo_metric_type == "deaths_rate"  ~ "New Deaths (Rate per person)"
    )
    
    month_label <- format(floor_date(input$geo_month, "month"), "%b %Y")
    
    pal <- colorBin(
      palette  = "YlOrRd",
      domain   = map_dat$metric_value,
      bins     = 7,
      na.color = "transparent"
    )
    
    leaflet(map_dat) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(metric_value),
        weight = 0.5,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~htmltools::HTML(
          paste0(
            "<strong>", ifelse(is.na(location), name, location), "</strong><br/>",
            month_label, "<br/>",
            metric_label, ": ",
            scales::label_number(scale_cut = cut_short_scale())(metric_value)
          )
        ),
        labelOptions = labelOptions(
          style = list("font-size" = "11px"),
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal     = pal,
        values  = ~metric_value,
        opacity = 0.7,
        title   = metric_label,
        position = "bottomright"
      )
  })
  
  # PANEL 3: SUMMARY TABLE (SEPARATE TAB)
  
  table_monthly_data <- reactive({
    req(input$table_month)
    
    selected_month <- floor_date(input$table_month, "month")
    
    df %>%
      mutate(month = floor_date(date, "month")) %>%
      filter(
        month == selected_month,
        !is.na(continent), continent != "",
        continent %in% input$continents_table
      ) %>%
      group_by(iso_code, location, continent, month) %>%
      summarise(
        new_cases  = sum(new_cases,  na.rm = TRUE),
        new_deaths = sum(new_deaths, na.rm = TRUE),
        population = max(population, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  
  output$geo_table <- renderTable({
    
    dat <- table_monthly_data()
    req(nrow(dat) > 0)
    
    # Total cases + total deaths for selected month
    totals <- df %>%
      mutate(month = floor_date(date, "month")) %>%
      filter(month == floor_date(input$table_month, "month")) %>%
      group_by(iso_code) %>%
      summarise(
        total_cases  = max(total_cases,  na.rm = TRUE),
        total_deaths = max(total_deaths, na.rm = TRUE),
        .groups = "drop"
      )
    
    dat %>%
      left_join(totals, by = "iso_code") %>%
      select(
        Country        = location,
        Continent      = continent,
        Population     = population,
        `New cases`    = new_cases,
        `New deaths`   = new_deaths,
        `Total cases`  = total_cases,
        `Total deaths` = total_deaths
      ) %>%
      arrange(desc(`Total cases`)) %>%
      slice_head(n = 10) %>%
      
      # Format numbers: commas + remove decimals
      mutate(
        Population     = format(Population,     big.mark = ",", scientific = FALSE),
        `New cases`    = format(`New cases`,    big.mark = ",", scientific = FALSE),
        `New deaths`   = format(`New deaths`,   big.mark = ",", scientific = FALSE),
        `Total cases`  = format(`Total cases`,  big.mark = ",", scientific = FALSE),
        `Total deaths` = format(`Total deaths`, big.mark = ",", scientific = FALSE)
      )
  })
  
  #  PANEL 4: Total COVID Deaths or Cases (Teammate barplot)

  covid_continent <- reactive({
    req(input$continent, input$input, input$metric_type)
    
    data <- covid_data %>%
      filter(continent == input$continent) %>%
      mutate(
        total_deaths = if_else(is.na(total_deaths), 0, total_deaths),
        total_cases  = if_else(is.na(total_cases),  0, total_cases)
      ) %>%
      group_by(location) %>%
      summarise(
        total_deaths = max(total_deaths, na.rm = TRUE),
        total_cases  = max(total_cases,  na.rm = TRUE),
        population   = max(population,   na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      mutate(
        death_rate = if_else(population > 0, total_deaths / population, NA_real_),
        case_rate  = if_else(population > 0, total_cases  / population, NA_real_)
      )
    
    # Decide which column we sort by, based on Deaths/Cases + Total/Rate
    sort_col <- dplyr::case_when(
      input$input == "Deaths" & input$metric_type == "Total" ~ "total_deaths",
      input$input == "Deaths" & input$metric_type == "Rate (per person)" ~ "death_rate",
      input$input == "Cases"  & input$metric_type == "Total" ~ "total_cases",
      input$input == "Cases"  & input$metric_type == "Rate (per person)" ~ "case_rate"
    )
    
    if (input$order == "desc") {
      data <- data %>% arrange(desc(.data[[sort_col]]))
    } else {
      data <- data %>% arrange(.data[[sort_col]])
    }
    
    data %>%
      slice_head(n = input$n_results)
  })
  
  
  # Plot: total or rate, deaths or cases, by country
  output$covid_plot1 <- renderPlot({
    req(input$input, input$metric_type)
    
    dat <- covid_continent()
    
    # Choose y variable, title, and axis label
    if (input$input == "Deaths") {
      if (input$metric_type == "Total") {
        y_var  <- "total_deaths"
        y_lab  <- "Total deaths"
        title_ <- "Total COVID-19 Deaths by Country"
      } else {  # Rate (per person)
        y_var  <- "death_rate"
        y_lab  <- "Deaths per person"
        title_ <- "COVID-19 Death Rate by Country"
      }
    } else {  # Cases
      if (input$metric_type == "Total") {
        y_var  <- "total_cases"
        y_lab  <- "Total cases"
        title_ <- "Total COVID-19 Cases by Country"
      } else {  # Rate (per person)
        y_var  <- "case_rate"
        y_lab  <- "Cases per person"
        title_ <- "COVID-19 Case Rate by Country"
      }
    }
    
    ggplot(dat, aes(x = reorder(location, .data[[y_var]]), y = .data[[y_var]])) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = title_,
        x     = "Country",
        y     = y_lab
      ) +
      theme_minimal()
  })
  
  #  PANEL 5: COVID Cases vs. Deaths (Teammate scatterplot)
  
  covid_location <- reactive({
    req(input$location, input$date)
    
    covid_data %>%
      select(location, new_cases, new_deaths, date) %>%
      filter(
        location == input$location,
        date >= input$date[1],
        date <= input$date[2]
      ) %>%
      mutate(
        new_cases  = if_else(is.na(new_cases), 0, new_cases),
        new_deaths = if_else(is.na(new_deaths), 0, new_deaths)
      )
  })
  
  
  output$covid_plot2 <- renderPlot({
    plot <- covid_location() %>%
      ggplot(aes(x = new_cases, y = new_deaths)) +
      geom_point(alpha = 0.5) +
      labs(
        title = str_wrap(
          paste("Relationship Between New COVID-19 Cases and New Deaths in", input$location),
          width = 50
        ),
        x = "New Cases per Day",
        y = "New Deaths per Day"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.text  = element_text(size = 11)
      )
    
    if (isTRUE(input$trend)) {
      plot <- plot + geom_smooth(method = "lm")
    }
    
    plot
  })
  
}
