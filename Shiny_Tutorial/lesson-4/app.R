# App for the lessons 3, 4 and 5
library(shiny)
library(maps)
library(mapproj)

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
                ), selected = "Percent White"
            ),
            sliderInput("range",
                        "Range of Interest:",
                        min = 0,
                        max = 100,
                        value = c(0, 100))
        ),
        mainPanel(
          textOutput("selected_var"),
          textOutput("selected_range")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Selection of the vars.
  output$selected_var <- renderText({
    paste("You have selected", input$var)
  })
  
  # Selection of the bins.
  output$selected_range <- renderText(({
    paste("You have selected a range between ", input$range[1], " and ", input$range[2])
  }))
}


# Run the application 
shinyApp(ui = ui, server = server)