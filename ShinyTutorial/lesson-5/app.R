# App for the lessons 3, 4 and 5
library(shiny)
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("censusVis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Create demographic maps with information from the 2010 US Census."),
            selectInput("var",
                label = "Choose a variable to display",    
                choices = c(
                    "Percent White",
                    "Percent Black",
                    "Percent Hispanic",
                    "Percent Asian"
                ),selected = "Percent White"
            ),
            sliderInput("bins",
                        "Range of Interest:",
                        min = 0,
                        max = 100,
                        value = c(0,100))
            ),
        mainPanel(
            #textOutput("selected_var"),
            #textOutput("selected_var_2")
            plotOutput("map")
        )
        # Show a plot of the generated distribution
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderPlot({
        data <- switch(input$var, 
                       "Percent White" = counties$white,
                       "Percent Black" = counties$black,
                       "Percent Hispanic" = counties$hispanic,
                       "Percent Asian" = counties$asian)
        
        colr <- switch(input$var, 
                       "Percent White" = "red",
                       "Percent Black" = "black",
                       "Percent Hispanic" = "blue",
                       "Percent Asian" = "darkgreen")
        
        labl <- switch(input$var, 
                       "Percent White" = "white",
                       "Percent Black" = "black",
                       "Percent Hispanic" = "hispanic",
                       "Percent Asian" = "asian")
        
        percent_map(data, colr, paste("%",labl),input$bins[1], input$bins[2])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
