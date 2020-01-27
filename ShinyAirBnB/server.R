library(shiny)
library(ggplot2)
library(dplyr)

# Load data for tabs
source("utils/utils.R")

server <- function(input, output, session) {
  filteredData <- reactive({
    if (input$mapviz == "score") {
      get_mean_score(input$score_var)
    } else if (input$mapviz == "price") {
      get_price_per_neighbours_with_dates(input$price_start_date, input$price_end_date)
    } else if (input$mapviz == "listings") {
      get_flat_count_in_a_date(input$number_of_flats_date)
    }
  })
  
  output$calendar_price_year_plot_error <-
    renderUI({
     if (input$from_year > input$to_year) {
       tags$span(
         style = "color:red; font-weight:bold",
         paste0('"From Year" (', input$from_year, ') must be before "To Year" (', input$to_year, ')')
       )
     } else {
       NULL
     }
   })

  output$calendar_price_year_plot <-
    renderPlot({
     if (input$from_year > input$to_year) return(NULL)
  
     material_spinner_show(session, "calendar_price_year")
  
     Sys.sleep(1.5) # sleep to show spinner example longer
  
     plot_input <- calendar %>%
       filter(date_year >= input$from_year & date_year <= input$to_year) %>%
       group_by(date_year, date_month) %>%
       summarise(average_price = mean(price, na.rm = TRUE))
  
     plot_out <- plot_input %>%
       ggplot(aes(x = paste(date_year, "-", date_month), y = average_price)) +
       geom_col(fill = "brown")
  
     material_spinner_hide(session, "calendar_price_year")
  
     plot_out
   })

  output$map <- renderLeaflet({
    map_data <- filteredData()
    
    labels <- getLabels(input$mapviz, map_data)
    pal <- getPal(input$mapviz, map_data)
    
    leaflet(neighbourhoods, options = providerTileOptions(minZoom = 11, maxZoom = 14)) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -3.66,
              lat = 40.43,
              zoom = 11) %>%
      addPoligonsToMap(map_data, pal, labels) %>%
      addLegendToMap(map_data, pal)
  })
  
  output$topNeighbourhoods <- renderPlot({
    plot_data <- filteredData()
    get_bar_plot(plot_data, input$mapviz)
  })
}