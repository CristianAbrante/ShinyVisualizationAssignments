server <- function(input, output) {
  output$calendar_price_year_error <- renderUI({
    if(input$from_year >= input$to_year){
      tags$span(
        style = "color:red; font-weight:bold",
        paste0('"From Year" (', input$from_year, ') must be before "To Year" (', input$to_year, ')')
      )
    } else {
      NULL
    }
  })

  output$calendar_price_year <-
    renderPlot({
                 if (input$from_year >= input$to_year) return(NULL)

                 material_spinner_show(session, "calendar_price_year")

                 Sys.sleep(1.5) # sleep to show spinner example longer

                 plot_input <- calendar %>%
                   group_by(month) %>%
                   aggregate(month = get_month_from_date(calendar$date), average_price = mean(calendar$price))

                 plot_out <- plot_input %>% ggplot(aes(x = month, y = price)) + geom_bar()

                 material_spinner_hide(session, "calendar_price_year")

                 plot_out
               })
}