second_tab_content <- material_tab_content(
  tab_id = "second_tab",
  material_row(
    height = 20,
    material_column(
      width = 3,
      material_card(
        title = "Settings",
        radioButtons("mapviz", "Map visualization:",
                     c("Prices" = "price",
                       "Score" = "score",
                       "Third Type" = "third"))
      ),
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

