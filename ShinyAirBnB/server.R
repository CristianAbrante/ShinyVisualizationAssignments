library(shiny)
library(ggplot2)
library(dplyr)

# Load data for tabs
source("utils/utils.R")

server <- function(input, output, session) {
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

  output$map_plot <- output$map <- renderLeaflet({
                                #Testing the load of the file as an SP obj
                                leaflet(neighbourhoods, options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
                                  addTiles() %>%
                                  addProviderTiles(providers$CartoDB.Positron) %>%
                                  setView(lng = -3.66, lat = 40.43, zoom = 11) %>%
                                  addPolygons(
                                    fillColor = ~pal(neigh_mean_prices[, 2]),
                                    weight = 2,
                                    opacity = 1,
                                    color = "white",
                                    dashArray = "3",
                                    fillOpacity = 0.3,
                                    highlight = highlightOptions(
                                      weight = 5,
                                      color = "#666",
                                      dashArray = "",
                                      fillOpacity = 0.5,
                                      bringToFront = TRUE),
                                    label = labels,
                                    labelOptions = labelOptions(
                                      style = list("font-weight" = "normal", padding = "3px 8px"),
                                      textsize = "15px",
                                      direction = "auto")) %>%
                                  addLegend(pal = pal, values = ~neigh_mean_prices[, 2], opacity = 0.7, title = NULL,
                                            position = "bottomright")
                              })
}