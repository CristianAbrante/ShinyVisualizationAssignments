# Title     : Main App for the User Interface
# Objective : Dashboard for selecting different views
# Created on: 26/01/2020

library(shiny)
library(shinymaterial)
library(ggplot2)
library(dplyr)

# Load data for tabs
load("utils/utils.R")

# Load tabs
load("views/first_tab.R")

# Load ui and server
load("ui.R")
load("server.R")

# Launch apñ
shinyApp(ui = ui, server = server)