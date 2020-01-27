library(shiny)
library(ggplot2)
library(dplyr)
library(shinymaterial)

# Load data for tabs
source("utils/utils.R")
source("utils/sentiment_analysis.R")

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
  
  output$calendar_price_year_plot <-
    renderPlot({
                 material_spinner_show(session, "calendar_price_year_plot")

                 Sys.sleep(1.5) # sleep to show spinner example longer

                 start_date_formatted <- as.Date(input$start_date, format = "%b %d, %Y")
                 end_date_formatted <- as.Date(input$end_date, format = "%b %d, %Y")

                 plot_input <- ifelse(start_date != "" & end_date != "",
                                      calendar %>%
                                        filter(as.Date(date, format = "%Y-%m-%d") >= start_date_formatted) %>%
                                        filter(as.Date(date, format = "%Y-%m-%d") <= end_date_formatted),
                                      calendar)

                 plot_out <- plot_input %>%
                   group_by(date_year, date_month) %>%
                   summarise(average_price = mean(price, na.rm = TRUE)) %>%
                   ggplot(aes(x = paste(date_year, "-", date_month), y = average_price)) +
                   geom_col(fill = "brown")

                 material_spinner_hide(session, "calendar_price_year_plot")

                 plot_out
               })

  output$calendar_price_year_plot <-
    renderPlot({
                 material_spinner_show(session, "calendar_price_year_plot")

                 Sys.sleep(1.5) # sleep to show spinner example longer

                 if (input$start_date != "" & input$end_date != "") {
                   start_date_formatted <- as.Date(input$start_date, format = "%b %d, %Y")
                   end_date_formatted <- as.Date(input$end_date, format = "%b %d, %Y")

                   plot_input <- calendar %>%
                     filter(as.Date(date, format = "%Y-%m-%d") >= start_date_formatted) %>%
                     filter(as.Date(date, format = "%Y-%m-%d") <= end_date_formatted)

                   plot_out <- plot_input %>%
                     group_by(date_year, date_month) %>%
                     summarise(average_price = mean(price, na.rm = TRUE)) %>%
                     ggplot(aes(x = paste(date_year, "-", date_month), y = average_price)) +
                     geom_col(fill = "brown") +
                     ggtitle("Price over the years") +
                     xlab("Date") +
                     ylab("Price") +
                     fancy_plot

                   material_spinner_hide(session, "calendar_price_year_plot")

                   plot_out
                 }
                 else {
                   plot_input <- calendar

                   plot_out <- plot_input %>%
                     group_by(date_year, date_month) %>%
                     summarise(average_price = mean(price, na.rm = TRUE)) %>%
                     ggplot(aes(x = paste(date_year, "-", date_month), y = average_price)) +
                     geom_col(fill = "brown") +
                     ggtitle("Price over the years") +
                     xlab("Date") +
                     ylab("Price") +
                     fancy_plot

                   material_spinner_hide(session, "calendar_price_year_plot")

                   plot_out
                 }
               })

  output$calendar_reviews_year_plot <-
    renderPlot({
                 material_spinner_show(session, "calendar_reviews_year_plot")

                 if (input$start_date != "" & input$end_date != "") {
                   start_date_formatted <- as.Date(input$start_date, format = "%b %d, %Y")
                   end_date_formatted <- as.Date(input$end_date, format = "%b %d, %Y")

                   plot_input <- reviews_dataset %>%
                     filter(as.Date(date, format = "%Y-%m-%d") >= start_date_formatted) %>%
                     filter(as.Date(date, format = "%Y-%m-%d") <= end_date_formatted)

                   plot_out <- plot_input %>%
                     group_by(date) %>%
                     summarise(reviews_number = n()) %>%
                     ggplot(aes(x = as.Date(date, format = "%Y-%m-%d"), y = reviews_number)) +
                     geom_point(color = "brown") +
                     geom_smooth(color = "black") +
                     ggtitle("Number of Reviews over the years") +
                     xlab("Date") +
                     ylab("Number of Reviews") +
                     fancy_plot

                   material_spinner_hide(session, "calendar_reviews_year_plot")
                   plot_out
                 }
                 else {
                   plot_input <- reviews_dataset

                   plot_out <- plot_input %>%
                     group_by(date) %>%
                     summarise(reviews_number = n()) %>%
                     ggplot(aes(x = as.Date(date, format = "%Y-%m-%d"), y = reviews_number)) +
                     geom_point(color = "brown") +
                     geom_smooth(color = "black") +
                     ggtitle("Number of Reviews over the years") +
                     xlab("Date") +
                     ylab("Number of Reviews") +
                     fancy_plot

                   material_spinner_hide(session, "calendar_reviews_year_plot")
                   plot_out
                 }
               })

  output$sentiment_analysis_positive_wordcloud <-
    renderPlot({
                 material_spinner_show(session, "sentiment_analysis_positive_wordcloud")
      
                 plot_input <- reviews_dataset

                 if (input$start_date != "" & input$end_date != "") {
                   start_date_formatted <- as.Date(input$start_date, format = "%b %d, %Y")
                   end_date_formatted <- as.Date(input$end_date, format = "%b %d, %Y")

                   plot_input <- plot_input %>%
                     filter(as.Date(date, format = "%Y-%m-%d") >= start_date_formatted) %>%
                     filter(as.Date(date, format = "%Y-%m-%d") <= end_date_formatted)
                 }
                 
                 # Get subsample
                 plot_input <- top_n(plot_input, 10000)
                 positive_frequent_words <- get_frequent_words(plot_input, TRUE)
                 material_spinner_hide(session, "sentiment_analysis_positive_wordcloud")
                 
                 ## Generate wordcloud
                 wordcloud2(data=frequent_words, size=1.6, color='random-dark')
               })

  output$sentiment_analysis_negative_wordcloud <-
    renderPlot({
                 material_spinner_show(session, "sentiment_analysis_negative_wordcloud")

                 plot_input <- reviews_dataset
                
                 if (input$start_date != "" & input$end_date != "") {
                   start_date_formatted <- as.Date(input$start_date, format = "%b %d, %Y")
                   end_date_formatted <- as.Date(input$end_date, format = "%b %d, %Y")
                   
                   plot_input <- plot_input %>%
                     filter(as.Date(date, format = "%Y-%m-%d") >= start_date_formatted) %>%
                     filter(as.Date(date, format = "%Y-%m-%d") <= end_date_formatted)
                 }
                
                 negative_frequent_words <- get_frequent_words(plot_input, FALSE)
                 material_spinner_hide(session, "sentiment_analysis_negative_wordcloud")
                
                 ## Generate wordcloud
                 wordcloud2(data=negative_frequent_words, size=1.6, color='random-dark')
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