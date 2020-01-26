second_tab_content <- material_tab_content(
  tab_id = "second_tab",
  material_row(
    material_column(
      width = 3,
      material_card(
        title = "Map visualization:",
        radioButtons("mapviz", " ",
                     c("Prices" = "price",
                       "Number of lisings" = "listings",
                       "Score" = "score"
                       )
                     )
        
      ),
      uiOutput("score_cat"),
    ),
    material_column(
      width = 9,
      material_card(
        title = "Map",
          leafletOutput("map")

      )
    )
  )
)

