library(shiny)
library(shinymaterial)

# Load data for tabs
source("utils/utils.R")

# Load tabs
source("views/first_tab.R")

# Wrap shinymaterial apps in material_page
ui <- material_page(
  title = "Madrid Airbnb Research",
   # Place side-nav in the beginning of the UI
  # Define views
  material_tabs(
    tabs = c(
      "Point 1" = "first_tab",
      "Point 2" = "second_tab"
    )
  ),
  # Define tab content
  first_tab_content,
  material_tab_content(
    tab_id = "second_tab",
    tags$h1("Second Tab Content")
  )
)
