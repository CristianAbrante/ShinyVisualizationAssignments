# Load of necessary libraries.

library(dplyr)
library(geojsonR)
library(leaflet)
library(geojsonio)
library(sf)
library(lubridate)
library(ggplot2)

####################################################################
#                     Dataset reading                              #
####################################################################

calendar_sample <-
  read.csv(
    './Data/calendar_sample.csv',
    encoding = "UTF-8",
    header = TRUE,
  )

calendar <- read.csv("Data/calendar_sample.csv")

# Remove NA
calendar <- na.omit(calendar)

reviews_dataset <- read.csv("Data/reviews_detailed_sample.csv", encoding = "UTF-8")

# Read the listings dataset
listings <- read.csv(
  './Data/listings.csv',
  encoding = "UTF-8",
  header = TRUE,
)

neighbourhoods_list <- read.csv(
  './Data/neighbourhoods.csv',
  encoding = "UTF-8",
  header = TRUE,
)

# We select the columns we want to load from listings_detailed
listCols = c(1:100)
listCols[1:106] = "NULL"
listCols[1] = NA	        # id
listCols[40] = NA	        # neighbourhood_cleansed
listCols[61] = NA	        # price
listCols[83] = NA	        # number_of_reviews
listCols[85] = NA	        # first_review
listCols[86] = NA	        # last_review
listCols[87] = "integer"	# review_scores_rating
listCols[88] = "integer"	# review_scores_accuracy
listCols[89] = "integer"	# review_scores_cleanliness
listCols[90] = "integer"	# review_scores_checkin
listCols[91] = "integer"	# review_scores_communication
listCols[92] = "integer"	# review_scores_location
listCols[93] = "integer"	# review_scores_value
listCols[106] = NA	      # reviews_per_month

#Reading the listings_detailed file with the price data
listings_detailed <-
  read.csv(
    file = "./Data/listings_detailed.csv",
    sep = ",",
    header = T,
    encoding = "UTF-8",
    colClasses = listCols
  )

## Load map data
ngb <- geojsonio::geojson_read("./Data/neighbourhoods_utf8.geojson", what = "sp")
ngb2 <- ngb[order(ngb$neighbourhood),] #We sort the neighbourhoods to have them in alphabetical order.

#We set the chosen dataset for the map
neighbourhoods <- ngb2

####################################################################
#                     Start graph                                  #
####################################################################

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

###################### Constants ##################################
date_picker_format <- "%b %d, %Y"
dataset_date_format <- "%Y-%d-%m"

####################################################################
#                     Price filtering                              #
####################################################################

# Agreagtion of calendar and listings.
calendar_and_listings <-
  inner_join(x = calendar_sample,
             y = listings,
             by = c("listing_id" = "id"))

calendar_and_listings <- calendar_and_listings %>%
  mutate(computed_price = as.numeric(gsub(
    "[$,]",
    "", as.character(calendar_and_listings$price.x)
  )))


# necessary for locale time.
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")


# function that returns the prices per neighbourhood given an start and end date.

get_price_per_neighbours_with_dates <-
  function(start_date, end_date) {
    data <- calendar_and_listings
    if (start_date != "" & end_date != "") {
      start_date_formatted <-
        as.Date(start_date, format = date_picker_format)
      end_date_formatted <-
        as.Date(end_date, format = date_picker_format)
      
      data <- data %>%
        filter(as.Date(date, format = dataset_date_format) >= start_date_formatted) %>%
        filter(as.Date(date, format = dataset_date_format) <= end_date_formatted)
    }
    
    data <- data %>% 
      group_by(neighbourhood) %>%
      summarise(avg_price = mean(computed_price))
    
    data <- left_join(neighbourhoods_list, data) %>% 
      mutate(avg_price = ifelse(is.na(avg_price), 0, avg_price)) %>%
      filter(neighbourhood != "Horcajo") %>%
      select(neighbourhood, avg_price)

    data[order(data$neighbourhood),]
  }

####################################################################
#                       Score type                                 #
####################################################################

get_mean_score <- function(score_type) {
  data <- listings_detailed %>% group_by(neighbourhood = neighbourhood_cleansed)
  if (score_type == "rating") {
    data %>% summarize(avg_score_rating = mean(review_scores_rating, na.rm = TRUE))
  } else if (score_type == "accuracy") {
    data %>% summarize(avg_score_accuracy = mean(review_scores_accuracy, na.rm = TRUE))
  } else if (score_type == "cleanliness") {
    data %>% summarize(avg_score_cleanliness = mean(review_scores_cleanliness, na.rm = TRUE))
  } else if (score_type == "checkin") {
    data %>% summarize(avg_score_checkin = mean(review_scores_checkin, na.rm = TRUE))
  } else if (score_type == "communication") {
    data %>% summarize(avg_score_communication = mean(review_scores_communication, na.rm = TRUE))
  } else if (score_type == "location") {
    data %>% summarize(avg_score_location = mean(review_scores_location, na.rm = TRUE))
  } else if (score_type == "value") {
    data %>% summarize(avg_score_value = mean(review_scores_value, na.rm = TRUE))
  } else {
    data %>% summarize(avg_score_rating = mean(review_scores_rating, na.rm = TRUE))
  }
}

####################################################################
#                    Number of apartments                          #
####################################################################

get_flat_count_in_a_date <- function(date) {
  data <- listings_detailed
  
  if (date != "") {
    date_formatted <-
      as.Date(date, format = date_picker_format)
    data <-
      data %>% filter(as.Date(first_review, format = dataset_date_format) <= date_formatted)
  }
  data <- data %>% 
    group_by(neighbourhood = neighbourhood_cleansed) %>%
    summarise(number_of_flats = n())
  
  data <- left_join(neighbourhoods_list, data) %>% 
    mutate(number_of_flats = ifelse(is.na(number_of_flats), 0, number_of_flats)) %>%
    filter(neighbourhood != "Horcajo") %>% 
    select(neighbourhood, number_of_flats)
  
  data[order(data$neighbourhood),]
}

####################################################################
#                         Bar plot                                 #
####################################################################

get_plot_label <- function(operation_type) {
  if (operation_type == "price") {
    "Price per night"
  } else if (operation_type == "score") {
    "Rating score"
  } else if (operation_type == "listings") {
    "Number of flats"
  }
}

get_bar_plot <- function(data, operation_type) {
  formatted_data <- data %>% arrange(desc(data[[2]])) %>% top_n(5)
  
  ggplot(data = formatted_data, aes(x = reorder(neighbourhood, -formatted_data[[2]]), y = formatted_data[[2]])) +
    geom_bar(stat ="identity", fill = "#ff3e4c") +
    labs(x = "Top neighbourhoods", y = get_plot_label(operation_type))
}

####################################################################
#                   cloropleth helpers                             #
####################################################################

get_label_html <- function(operation_type) {
  base <- "<strong>%s</strong><br/>%g"
  if (operation_type == "listings") {
    paste(base, " flats")
  } else if (operation_type == "score") {
    paste(base, " ")
  } else {
    paste(base, " E")
  }
}

getLabels <- function(operation_type, data) {
  sprintf(get_label_html(operation_type),
          data[[1]],
          round(data[[2]], 2)) %>% lapply(htmltools::HTML)
}

getBins <- function(operation_type, data) {
  if (operation_type == "price") {
    c(0, 50, 100, 200, 300, 400, 500, Inf)
  } else if (operation_type == "score") {
    numeric_data <- data[[2]]
    # example: c(75, 80, 85, 90, 95, 100)
    bins <-
      seq(min(numeric_data), max(numeric_data), (max(numeric_data) - min(numeric_data)) / 5)
  } else if (operation_type == "listings") {
    c(0, 50, 100, 200, 300, 500, 1000, 1500, Inf)
  }
}

getPal <- function(operation_type, data) {
  numeric_data <- data[[2]]
  bins <- getBins(operation_type, data)
  colorBin("YlOrRd", domain = round(numeric_data, 2), bins = bins)
}

addPoligonsToMap <- function(initialOptions, data, pal, labels) {
  addPolygons(
    initialOptions,
    fillColor = ~ pal(data[[2]]),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.3,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.5,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  )
}

addLegendToMap <- function(initialOptions, data, pal) {
  addLegend(
    initialOptions,
    pal = pal,
    values = ~ data[[2]],
    opacity = 0.7,
    title = NULL,
    position = "bottomright"
  )
}

fancy_text_title <- theme(title = element_text(color = "chocolate",
                                                      size = 14, face = "bold", margin = 3, line = 2))

fancy_plot <-
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
  ) +
  fancy_text_title
