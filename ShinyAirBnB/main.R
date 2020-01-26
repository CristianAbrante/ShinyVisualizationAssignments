# Title     : Main App for the User Interface
# Objective : Dashboard for selecting different views
# Created on: 26/01/2020

library(shiny)
library(shinymaterial)
library(ggplot2)
library(dplyr)

# Load data for tabs
source("utils/utils.R")

# Load tabs
source("views/first_tab.R")

# Load ui and server
source("ui.R")
source("server.R")

# Launch apñ
shinyApp(ui = ui, server = server)