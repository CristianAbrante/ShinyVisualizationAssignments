library(shiny)
library(shinymaterial)
library(leaflet)

# Load data for tabs
source("utils/utils.R")

# Load tabs
source("views/first_tab.R")
source("views/second_tab.R")

# Wrap shinymaterial apps in material_page
ui <- material_page(
  title = "Madrid Airbnb Research",
   # Place side-nav in the beginning of the UI
  # Define views
  material_tabs(
    tabs = c(
      "Bar Plot - Availability" = "first_tab",
      "Maps" = "second_tab"
    )
  ),
  # Define tab content
  first_tab_content,
  second_tab_content
)
