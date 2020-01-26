library(shiny)
library(ggplot2)
library(dplyr)

# Load data for tabs
source("utils/utils.R")

server <- function(input, output, session) {
  output$calendar_price_year_plot_error <-
    renderUI({
               if (input$from_year >= input$to_year) {
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
                 if (input$from_year >= input$to_year) return(NULL)

                 material_spinner_show(session, "calendar_price_year")

                 Sys.sleep(1.5) # sleep to show spinner example longer

                 plot_input <- calendar %>%
                   group_by(date_year, date_month) %>%
                   summarise(average_price = mean(price, na.rm = TRUE))

                 plot_out <- plot_input %>%
                   ggplot(aes(x = paste(date_year, "-", date_month), y = average_price)) +
                   geom_col(fill = "brown")

                 material_spinner_hide(session, "calendar_price_year")

                 plot_out
               })
}