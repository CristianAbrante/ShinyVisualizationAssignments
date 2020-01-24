#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(geojsonR)
library(leaflet)
library(geojsonio)
library(ggplot2)

#Loaded using geosonR
neighbourhoods = FROM_GeoJson(url_file_string = "./Data/neighbourhoods.geojson")

#Loaded using readOGR
neighbourhoods <- rgdal::readOGR("./Data/neighbourhoods.geojson")

#Loaded reading the file as a text file and removing Linejumps
GeoData <- readLines("./Data/neighbourhoods.geojson") %>% paste(collapse = "\n")

#Reading the file with the price data 
listings <- read.csv(file = "./Data/listings.csv",sep = ",",header = T) 
calendar <- read.csv(file = "./Data/calendar.csv",sep = ",",header = T) 

#We extract the price mean per neighb.
neigh_mean_prices <- aggregate(listings$price, list(listings$neighbourhood), mean)

#Testing leaflet
m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

m %>% addProviderTiles(providers$CartoDB.Positron)
m  # Print the map

#We create the labels for the map
labels <- sprintf(
    "<strong>%s</strong><br/>%g €",
    neigh_mean_prices$Group.1, round(neigh_mean_prices$x, 2)
) %>% lapply(htmltools::HTML)

# We set some clours for our scale
bins <- c(0, 50, 100, 200, 300, 400, 500, Inf)
pal <- colorBin("YlOrRd", domain = neigh_mean_prices$x, bins = bins)

#Testing the load of the file as an SP obj
mainmap <- leaflet(neighbourhoods, options = providerTileOptions(minZoom = 10, maxZoom = 14))  %>%
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    setView(lng = -3.66, lat = 40.43, zoom = 11) %>% 
    addPolygons( 
        fillColor = ~pal(neigh_mean_prices[,2]),
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
    addLegend(pal = pal, values = ~neigh_mean_prices[,2], opacity = 0.7, title = NULL,
              position = "bottomright")

    #Testing with the raw GeoJSON file
# leaflet(options = providerTileOptions(minZoom = 10, maxZoom = 15)) %>% 
#     setView(lng = -3.6, lat = 40.3, zoom = 11) %>% 
#     addProviderTiles(providers$CartoDB.Positron) %>% 
#     addTiles() %>%
#     addGeoJSON(topoData, weight = 1, color = "#00DD30", fill = TRUE, stroke = TRUE)

# Good graph to see the price differences:
# ggplot(listings, aes(x=price)) + geom_density() + scale_x_continuous(trans = 'log10')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Madrid's AirBnBs Price per neighbourhood"),

    # Tabbed views
    tabsetPanel(type = "tabs",
        #First tab: Choropleth
        tabPanel("Chloropleth",
            sidebarLayout(
                sidebarPanel(
                    "Here you can see the differece in average prices from all the different neighbourhoods of Madrid"
                    # sliderInput("zoom",
                    #             "Amount of zoom:",
                    #             min = 10,
                    #             max = 15,
                    #             value = 11)
                ),
                # Show the Map
                mainPanel(
                   mainmap
                )
            )
        ),
        #Second tab: Bar Plot
        tabPanel("Month by Month",
            sidebarLayout(
                sidebarPanel(
                    "Second pannel"
                ),
                # Show a plot of the generated distribution
                mainPanel(
                    mainmap
                )
            )
        )
    )
    #End of sidebar panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        mainmap
    }, height = 2000)
}

# Run the application 
shinyApp(ui = ui, server = server)
