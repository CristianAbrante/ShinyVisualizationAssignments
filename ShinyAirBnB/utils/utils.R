library(dplyr)

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
