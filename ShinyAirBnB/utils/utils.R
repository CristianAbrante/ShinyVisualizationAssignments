calendar <- read.csv("Data/calendar_sample.csv")

get_year_from_date <- function(date) {
  format(as.Date(date), "%Y")
}

get_month_from_date <- function(date) {
  format(as.Date(date), "%M")
}

## Preprocess dataset
calendar <- calendar %>% mutate(date_month = get_month_from_date(calendar$date))