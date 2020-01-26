library(dplyr)
library(geojsonR)
library(leaflet)
library(geojsonio)
library(sf)
library(lubridate)

calendar <- read.csv("Data/calendar_sample.csv")

# Remove NA
calendar <- na.omit(calendar)

get_year_from_date <- function(date) {
  format(as.Date(date), "%Y")
}

get_month_from_date <- function(date) {
  format(as.Date(date), "%m")
}

transform_price <- function(price) {
  ## Remove dollar sign
  price_string <- sub(".", "", price)
  # Replace commas
  price_string <- gsub(",", "", price_string)

  as.numeric(price_string)
}

## Preprocess dataset
calendar <- calendar %>% mutate(date_year = get_year_from_date(calendar$date))
calendar <- calendar %>% mutate(date_month = get_month_from_date(calendar$date))
calendar <- calendar %>% mutate(price = as.numeric(transform_price(calendar$price)))


## Load map data
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

