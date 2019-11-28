# App for the Lesson 1

library(shiny)

# Define UI ----
ui <- fluidPage(
    tags$head(tags$style(
        HTML(
            '
        #main-panel {
            color: white;
            background-color: #3f51b5;
            height: 100vh
        }

        #panel-left {
            color: black;
            background-color: #e3f2fd;
            height: 400px;
        }

        #panel-right {
            color: black;
            background-color: #ffebee;
            height: 400px;
            width: 65%
        }

        #panel-bottom {
            color: black;
            background-color: #b2ebf2;
            width: 100%;
            margin-bottom: 10px
        }

        body, label, input, button, select {
          font-family: "Arial";
        }'
        )
    )),
    id = "main-panel",
    titlePanel("My Shiny App"),
    sidebarLayout(
        sidebarPanel(
            id = "panel-left",
            h2("Installation"),
            p(
                "Shiny is available on CRAN, so you can install it in the usual way from your R console:"
            ),
            code('install.packages("shiny")'),
            br(),
            br(),
            br(),
            br(),
            img(
                src = "rstudio.png",
                height = 70,
                width = 200
            ),
            br(),
            "Shiny is a product of ",
            span("RStudio", style = "color:blue")
        ),
        mainPanel(
            id = "panel-right",
            h3("Introducing Shiny"),
            p(
                "Shiny is a new package from RStudio that makes it ",
                em("incredibly easy "),
                "to build interactive web applications with R."
            ),
            br(),
            p(
                "For an introduction and live examples, visit the ",
                a("Shiny homepage.",
                  href = "http://shiny.rstudio.com")
            ),
            br(),
            h2("Features"),
            p(
                "- Build useful web applications with only a few lines of code—no JavaScript required."
            ),
            p(
                "- Shiny applications are automatically 'live' in the same way that ",
                strong("spreadsheets"),
                " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser."
            )
        )
    ),
    mainPanel(
        id = "panel-bottom",
        "This is what a second main panel would look like if it didn't have the SidebarLayout before it.",
        p("Im a paragrapgh inside the second main panel.")
    ),
    p(
        "You can also create separated paragraphs and the rest of elemets outside the function without problems."
    ),
    p(
        "However paragraphs may look a bit weird if they are not inside other function like 'main panel' (see precious paragraph)"
    )
)

# Define server logic ----
server <- function(input, output) {
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
