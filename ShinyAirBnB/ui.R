library(shiny)
library(shinymaterial)
library(leaflet)

# Load data for tabs
source("utils/utils.R")

# Load tabs
source("views/first_tab.R")
source("views/second_tab.R")
source("views/third_tab.R")

# Wrap shinymaterial apps in material_page
ui <- material_page(
  title = "Madrid Airbnb Research",
   # Place side-nav in the beginning of the UI
  # Define views
  material_tabs(
    tabs = c(
      "Point 1" = "first_tab",
      "Point 2" = "second_tab",
      "Third tab" = "third_tab"
    )
  ),
  # Define tab content
  first_tab_content,
  second_tab_content,
  third_tab_content
)
