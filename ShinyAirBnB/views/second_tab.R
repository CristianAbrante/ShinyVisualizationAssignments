second_tab_content <- material_tab_content(
  tab_id = "second_tab",
  material_row(
    material_column(
      width = 3,
      material_card(title = "Map visualization:",
                    radioButtons(
                      "mapviz",
                      " ",
                      c(
                        "Score" = "score",
                        "Prices" = "price",
                        "Number of flats" = "listings"
                      )
                    )),
      material_card(
        title = "Score controls",
        selectInput(
          "score_var",
          "Variable:",
          c(
            "Rating" = "rating",
            "Accuracy" = "accuracy",
            "Cleanliness" = "cleanliness",
            "Checkin" = "checkin",
            "Communication" = "communication",
            "Location" = "location",
            "Value" = "value"
          ))),
      material_card(
        title = "Price controls",
        material_date_picker(
          input_id = "price_start_date",
          color = "red",
          label = "Start date"
        ),
        material_date_picker(
          input_id = "price_end_date",
          color = "red",
          label = "End date"
        )
      ),
      material_card(
        title = "Number of flats controls",
        material_date_picker(
          input_id = "number_of_flats_date",
          color = "red",
          label = "Date"
        )
      )
    ),
    material_column(
      width = 9,
      material_card(title = "Map",
                    leafletOutput("map")),
      material_card(title = "Top neighbourhoods",
                    plotOutput("topNeighbourhoods"))
    )
  ))
  

