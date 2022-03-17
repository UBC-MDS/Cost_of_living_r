library(tidyverse)
library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(here)
library(ggthemes)


#read data
data_df <- read.csv("data/processed_data.csv")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

price_subset <- list(
  list(label="Total Monthly cost", value="all"),
  list(label="Basic Groceries", value= "grocery_for_one_person"),
  list(label="Childcare", value ="childcare_for_one_child"),
  list(label = "Entertainment", value ="entertainment"),
  list(label ="Fitness", value ="fitness"),
  list(label="Monthly Rent", value="rent_for_one_person"),
  list(label="Public Transport", value="transportation_public"),
  list(label="Shopping", value="shopping"),
  list(label="Utilities", value="utility_bills")
)

price_subset_2 <- data.frame(
  title=c("Total Monthly Cost",
          "Basic Groceries",
          "Childcare", 
          "Entertainment",
          "Fitness", 
          "Monthly Rent",
          "Public Transport",
          "Shopping",
          "Utilities") , 
  feature=c("all",
            "grocery_for_one_person",
            "childcare_for_one_child",
            "entertainment",
            "fitness",
            "rent_for_one_person",
            "transportation_public",
            "shopping",
            "utility_bills"))

cities <- unique(data_df$city)
regions <- unique(data_df$region)


SIDEBAR_STYLE <- list(
  "position"= "fixed",
  "top"= 0,
  "left"= 0,
  "bottom"= 0,
  "width"= "15rem",
  "padding"= "2rem 1rem",
  "background-color"= "#15599e",
  "color"= "white"
)


sidebar <- htmlDiv(list(
  htmlH5("Tech Worker Mental Health Tracker"),
  htmlHr(),
  htmlP(
    "Filter By: ", className="lead"
  ), htmlDiv(list(
    htmlLabel(list('Select cities: ', dccDropdown(id = 'cities',
                                              options = data_df %>%select(city) %>% pull() %>% purrr::map(function(col) list(label = col, value = col)),
                                              multi = TRUE, 
                                              value=c('Vancouver','Calgary','New York City','London'),
                                              style = list('color'= 'black')
                                              ))),htmlHr(),
    htmlLabel(list('Select regions: ', dccDropdown(id = 'regions', 
                                              options = data_df %>%select(region) %>% pull() %>% purrr::map(function(col) list(label = col, value = col)),
                                              value='Canada',
                                              style = list('color'= 'black')
    ))),
    htmlHr(),
    htmlLabel(list('Select monthly costs: ', dccDropdown(id = 'monthly-cost', 
                                              options = price_subset,
                                              value='all',
                                              style = list('color'= 'black')
    ))),
    htmlHr(),
    htmlLabel(list('Monthly Salary: ', dccInput(id="expected-earnings", 
                                                type="number",
                                                placeholder=2000,
                                                value=2000
    )))))), style=SIDEBAR_STYLE)



CONTENT_STYLE <- list("margin-left" = "12rem","margin-right" = "2rem","padding" = "2rem 1rem")

content <- dbcContainer(htmlDiv(list(dbcRow(list(dbcCol(dccGraph(id = 'monthly_plot', style= list(
  'border-width' = '0'))),
  dbcCol(dccGraph(id = 'property_bar', style= list(
    'border-width' = '0'))))),
  htmlBr(),
  dbcRow(list(dbcCol(dccGraph(id = 'saving_bar', style= list(
    'border-width' = '0'))),
    dbcCol(dccGraph(id = 'map_plot', style= list(
      'border-width' = '0')))))), style = CONTENT_STYLE))

app$layout(htmlDiv(list(sidebar, content)))

#saving_bar 

app$callback(
  output('saving_bar', 'figure'),
  list(input('cities', 'value'),
       input('expected-earnings', 'value')),
  function(cities, earning) {
    subset <- data_df %>%
      filter(city %in% cities) %>%
      mutate(surplus = earning - all)
    p <- ggplot(subset, aes(
      x = city,
      y = surplus, 
      fill = city))+
      geom_col(show.legend = FALSE)  +
      labs(x = "Monthly Surplus (USD)", y = "City", 
           title = "Salary minus the monthly cost of living")
    ggplotly(p)
  }
)

#property bar 

app$callback(
  output('property_bar', 'figure'),
  list(input('cities', 'value')),
  function(city_name) {
    subset <- data_df %>%
      filter(city %in% city_name) 
    
    p <- ggplot(subset, aes(
      x = city,
      y = property_price, 
      fill = city))+
      geom_col(show.legend = FALSE)  +
      labs(y = "Property Price (USD)", x = "City", 
           title = "Property Prices in selected cities")
    ggplotly(p)
  }
)

#map plot 

app$callback(
  output('map_plot', 'figure'),
  list(input('cities', 'value'),
       input('monthly-cost', 'value')),
  function(city_name,cost_subset) {
    subset <- data_df %>% 
      filter(city %in% city_name) 
    
    y_title <- paste(price_subset_2$title[price_subset_2$feature == cost_subset], ": ")
    g <- list(   
      showframe = T,
      showcoastlines = T,
      showcountries = T,
      showland = T,
      landcolor = toRGB("grey90")
    )
    
    subset$hovertext <- paste("City:",
                              subset$city, 
                              "<br>",
                              y_title,
                              subset[[cost_subset]])
    
    p <- plot_geo(subset)%>% 
      add_trace(
        type = 'scatter',
        mode = 'markers',
        x = ~longitude, 
        y = ~latitude, 
        text = ~city,
        color = ~city,
        marker = list(size = 20),
        hovertext = ~hovertext,
        hoverinfo = 'text'
      ) %>% 
      layout(geo = g, width = 700, height = 290)
    ggplotly(p)
  }
)

#monthly_plot

app$callback(
  output('monthly_plot', 'figure'),
  list(input('cities', 'value'),
       input('monthly-cost', 'value')),
  function(city_name,cost_subset) {
    subset <- data_df %>% 
      filter(city %in% city_name)
    y_title <- price_subset_2$title[price_subset_2$feature == cost_subset]
    
    p <- ggplot(subset, aes(x = city,
                            y = !!sym(cost_subset),
                            fill = city
    )) +
      labs(x = 'City', y = y_title) +
      geom_col() 
    ggplotly(p)
  }
)


app$run_server(host = '0.0.0.0')
