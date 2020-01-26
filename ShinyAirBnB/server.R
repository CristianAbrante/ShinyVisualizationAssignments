library(shiny)
library(ggplot2)
library(dplyr)

# Load data for tabs
source("utils/utils.R")

server <- function(input, output, session) {
  
  mapview <- reactive({
    switch(input$mapviz,
       "price" = "rprice",
       "score" = "rscore",
       "third" = "rthird",
       "rprice")
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
    #Set the aspects of the map that don't change
    leaflet(neighbourhoods, options = providerTileOptions(minZoom = 11, maxZoom = 14)) %>%
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
  
  observe({
    proxy <- leafletProxy("map", data = neighbourhoods)
    
    if(input$mapviz=="price"){
      print("R dates:")
      price_per_neighbourhood <-
        get_price_per_neighbours_with_dates(input$price_start_date, input$price_end_date)
      
      #We create the labels for the map
      labels <- sprintf(
        "<strong>%s</strong><br/>%g €",
        price_per_neighbourhood$neighbourhood, round(price_per_neighbourhood$avg_price, 2)
      ) %>% lapply(htmltools::HTML)
      
      # We set some clours for our scale
      bins <- c(0, 50, 100, 200, 300, 400, 500, Inf)
      pal <- colorBin("YlOrRd", domain = price_per_neighbourhood$avg_price, bins = bins)
      
      #We set the chosen dataset for the map
      neighbourhoods <- ngb2
      
      proxy %>%
        clearShapes() %>%
        clearControls() %>% 
        addPolygons( 
          fillColor = ~pal(price_per_neighbourhood$avg_price),
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
        addLegend(pal = pal, values = ~price_per_neighbourhood$avg_price, opacity = 0.7, title = NULL, position = "bottomright")
    } else {
      if(input$mapviz=="score"){
        score_type = input$score_var 
        if(is.null(score_type)){
          score_type = "rating"
        } 
        sct <- switch(score_type,
               "rating" = neigh_mean_scores_rating,
               "accuracy" = neigh_mean_scores_accuracy,
               "cleanliness" = neigh_mean_scores_cleanliness,
               "checkin" = neigh_mean_scores_checkin,
               "communication" = neigh_mean_scores_communication,
               "location" = neigh_mean_scores_location,
               "value" = neigh_mean_scores_value
               )
        #We create the label
        labels2 <- sprintf(
          paste("<strong>%s</strong><br/>%g avg.",score_type,"score"),
          sct$Group.1, sct$x
        ) %>% lapply(htmltools::HTML)
        
        #bins = seq(min(neigh_mean_scores$x),100,5) or similar
        bins2 <- seq(min(sct$x),max(sct$x),(max(sct$x)-min(sct$x))/5)#c(75, 80, 85, 90, 95, 100)
        pal2 <- colorBin("YlOrRd", domain = round(sct$x, 2), bins = bins2)
        
        print("R score type:")
        print(score_type)
        proxy %>%
          clearShapes() %>% 
          clearControls() %>% 
          addPolygons(
            fillColor = ~pal2(sct[, 2]),
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
            label = labels2,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto")) %>%
          addLegend(pal = pal2, values = ~sct[,2], opacity = 0.7, title = NULL,
                    position = "bottomright")
      } else {
        #3rd installment
        print("R dates:")
          price_per_neighbourhood <-
            get_price_per_neighbours_with_dates(input$price_start_date, input$price_end_date)
          
          #We create the labels for the map
          labels <- sprintf(
            "<strong>%s</strong><br/>%g €",
            price_per_neighbourhood$neighbourhood, round(price_per_neighbourhood$avg_price, 2)
          ) %>% lapply(htmltools::HTML)
          
          # We set some clours for our scale
          bins <- c(0, 50, 100, 200, 300, 400, 500, Inf)
          pal <- colorBin("YlOrRd", domain = price_per_neighbourhood$avg_price, bins = bins)
          
          #We set the chosen dataset for the map
          neighbourhoods <- ngb2
          
          proxy %>%
            clearShapes() %>%
            clearControls() %>% 
            addPolygons( 
              fillColor = ~pal(price_per_neighbourhood$avg_price),
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
            addLegend(pal = pal, values = ~price_per_neighbourhood$avg_price, opacity = 0.7, title = NULL, position = "bottomright")
      }
    }
  })
  
  output$score_cat <- renderUI({
    if(input$mapviz=="score"){
      material_column(
        width = 12,
      selectInput("score_var", "Variable:",
                  c("Rating" = "rating",
                    "Accuracy" = "accuracy",
                    "Cleanliness" = "cleanliness",
                    "Checkin" = "checkin",
                    "Communication" = "communication",
                    "Location" = "location",
                    "Value" = "value")))
    } else {
        material_column(
          width = 12,
          material_date_picker(
            input_id = "price_start_date",
            color = "red",
            label = "Start date"
          ),
          textOutput("price_start_date"),
          material_date_picker(
            input_id = "price_end_date",
            color = "red",
            label = "End date"
          )
        )
    }
  })

  output$cloropleth <- 
    renderLeaflet({
      price_per_neighbourhood <-
        get_price_per_neighbours_with_dates(input$price_start_date, input$price_end_date)
      
      #We create the labels for the map
      labels <- sprintf(
        "<strong>%s</strong><br/>%g €",
        price_per_neighbourhood$neighbourhood, round(price_per_neighbourhood$avg_price, 2)
      ) %>% lapply(htmltools::HTML)
      
      # We set some clours for our scale
      bins <- c(0, 50, 100, 200, 300, 400, 500, Inf)
      pal <- colorBin("YlOrRd", domain = price_per_neighbourhood$avg_price, bins = bins)
      
      #We set the chosen dataset for the map
      neighbourhoods <- ngb2
      
      leaflet(
        neighbourhoods, 
        options = providerTileOptions(minZoom = 10, maxZoom = 14))  %>%
        addTiles() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>% 
        setView(lng = -3.66, lat = 40.43, zoom = 11) %>% 
        addPolygons( 
          fillColor = ~pal(price_per_neighbourhood$avg_price),
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
        addLegend(pal = pal, values = ~price_per_neighbourhood$avg_price, opacity = 0.7, title = NULL, position = "bottomright")
    })
}