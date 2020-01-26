library(dplyr)
library(geojsonR)
library(leaflet)
library(geojsonio)
library(sf)
library(lubridate)

calendar_sample <-
  read.csv(
    './Data/calendar_sample.csv',
    encoding = "UTF-8",
    header = TRUE,
  )

listings <- read.csv(
  './Data/listings.csv',
  encoding = "UTF-8",
  header = TRUE,
)

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

# Agreagtion of calendar and listings.
calendar_and_listings <- inner_join(x = calendar_sample, y = listings, by = c("listing_id" = "id"))
calendar_and_listings <- mutate(calendar_and_listings, computed_price = as.numeric(gsub("[$,]","",as.character(calendar_and_listings$price.x))))

# necessary for locale time.
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C");
# function that returns the prices per neighbourhood given an start and end date.
get_price_per_neighbours_with_dates <- function(start_date, end_date) {
  if (start_date != "" & end_date != "") {
    start_date_formatted <-  as.Date(start_date, format = "%b %d, %Y");
    end_date_formatted <- as.Date(end_date, format = "%b %d, %Y");
    calendar_and_listings <- calendar_and_listings %>% 
      filter(as.Date(date, format="%Y-%m-%d") >= start_date_formatted) %>% 
      filter(as.Date(date, format="%Y-%m-%d") <= end_date_formatted) %>% 
      group_by(neighbourhood) %>% 
      summarise(avg_price = mean(computed_price))
  } else {
    calendar_and_listings %>%  
      group_by(neighbourhood) %>% 
      summarise(avg_price = mean(computed_price))
    
  }
}

## Preprocess dataset
calendar <- calendar %>% mutate(date_year = get_year_from_date(calendar$date))
calendar <- calendar %>% mutate(date_month = get_month_from_date(calendar$date))
calendar <- calendar %>% mutate(price = as.numeric(transform_price(calendar$price)))

## Load map data
ngb <- geojsonio::geojson_read("./Data/neighbourhoods_utf8.geojson", what = "sp")
ngb2 <- ngb[order(ngb$neighbourhood),] #We sort the neighbourhoods to have them in alphabetical order.
 
# We select the columns we want to load from listings_detailed
listCols = c(1:100)
listCols[1:106] = "NULL"
listCols[1] = NA	#id
listCols[5] = NA	#name
listCols[6] = NA	#summary
listCols[10] = NA	#neighborhood_overview
listCols[23] = NA	#host_since
listCols[40] = NA	#neighbourhood_cleansed
listCols[49] = NA	#latitude
listCols[50] = NA	#longitude
listCols[52] = NA	#property_type
listCols[61] = NA	#price
listCols[83] = NA	#number_of_reviews
listCols[87] = "integer"	#review_scores_rating
listCols[106] = NA	#reviews_per_month

#Reading the file with the price data
listings <- read.csv(file = "./Data/listings_detailed.csv", sep = ",", header = T, encoding = "UTF-8", colClasses = listCols)
scal <- read.csv(file = "./Data/calendar_sample.csv", sep = ",", header = T)

#We transform the prices to num values
listings$numprices =  as.numeric(sub("$","",sub(".","",sub(",","",listings$price)))) 

#We extract the review score avg per neighbourhood.
neigh_mean_scores <- aggregate(listings$review_scores_rating, list(listings$neighbourhood_cleansed), mean, na.rm=T)

#We extract the price mean per neighbourhood.
neigh_mean_prices <- aggregate(listings$numprices, list(listings$neighbourhood_cleansed), mean)

#We compare length and (separately) that both vectors have the same components 
length(ngb2@data[["neighbourhood"]]) == length(neigh_mean_prices$Group.1)

#We create the labels for the map
labels <- sprintf(
  "<strong>%s</strong><br/>%g €",
  neigh_mean_prices$Group.1, round(neigh_mean_prices$x, 2)
) %>% lapply(htmltools::HTML)

# We set some clours for our scale
bins <- c(0, 50, 100, 200, 300, 400, 500, Inf)
pal <- colorBin("YlOrRd", domain = neigh_mean_prices$x, bins = bins)

#We create the labels for the map 2
labels2 <- sprintf(
  "<strong>%s</strong><br/>%g avg. score",
  neigh_mean_scores$Group.1, round(neigh_mean_scores$x, 2)
) %>% lapply(htmltools::HTML)

#bins = seq(min(neigh_mean_scores$x),100,5) or similar
bins2 <- c(75, 80, 85, 90, 95, 100)
pal2 <- colorBin("YlOrRd", domain = neigh_mean_scores$x, bins = bins2)

#We set the chosen dataset for the map
neighbourhoods <- ngb2

