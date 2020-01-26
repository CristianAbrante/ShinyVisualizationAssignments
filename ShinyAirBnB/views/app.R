library(shiny)
library(geojsonR)
library(leaflet)
# library(geojsonio)
library(ggplot2)
library(sf)
library(lubridate)


#We load the geographical data
ngb <- geojsonio::geojson_read("../Data/neighbourhoods_utf8.geojson", what = "sp")
ngb2 <- ngb[order(ngb$neighbourhood),] #We sort the neighbourhoods to have them in alphabetical order.

#Reading the file with the price data 
listings <- read.csv(file = "../Data/listings.csv", sep = ",", header = T, encoding = "UTF-8")
# calendar <- read.csv(file = "./Data/calendar.csv", sep = ",", header = T)
scal <- read.csv(file = "../Data/calendar_sample.csv", sep = ",", header = T)

#We extract the price mean per neighbourhood.
neigh_mean_prices <- aggregate(listings$price, list(listings$neighbourhood), mean)

#To display our neighbourhoods in the map:
ngb2@data[["neighbourhood"]]

# To display the listing neighbourhood names
neigh_mean_prices$Group.1

#We can see that both lists have the same lengths and are in the same order.

#We create the labels for the map
labels <- sprintf(
  "<strong>%s</strong><br/>%g €",
  neigh_mean_prices$Group.1, round(neigh_mean_prices$x, 2)
) %>% lapply(htmltools::HTML)

# We set some clours for our scale
bins <- c(0, 50, 100, 200, 300, 400, 500, Inf)
pal <- colorBin("YlOrRd", domain = neigh_mean_prices$x, bins = bins)

#We set the chosen dataset for the map
neighbourhoods <- ngb2


# Good graph to see the price differences:
#ggplot(listings, aes(x=price)) + geom_density() + scale_x_continuous(trans = 'log10')
#ggplot(calendar, aes(x = month(as.POSIXlt(date, format="%Y-%m-%d")))) + geom_bar()

# Define UI for application that draws a histogram
ui <- fluidPage(

# Application title
titlePanel("Madrid's AirBnBs Price per neighbourhood"),

# Tabbed views
tabsetPanel(type = "",
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
                         tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
                         leafletOutput("map")
                       ),
                     ),
                     style = "height:80%;"),
            #div(style = "height:100px;background-color: blue;", "Topright"),
            #Second tab: Bar Plot
            tabPanel("Month by Month",
                     sidebarLayout(
                       sidebarPanel(
                         "Second pannel"
                       ),
                       # Show a plot of the generated distribution
                       mainPanel(

                       #leafletOutput("distPlot")
                       )
                     )
            )
)
#End of sidebar panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({
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

  output$distPlot <- renderPlot({
                                  # # generate bins based on input$bins from ui.R
                                  # x    <- faithful[, 2]
                                  # bins <- seq(min(x), max(x), length.out = input$bins + 1)
                                  #
                                  # # draw the histogram with the specified number of bins
                                  # hist(x, breaks = bins, col = 'darkgray', border = 'white')

                                }, height = 2000)
}

# Run the application 
shinyApp(ui = ui, server = server)
