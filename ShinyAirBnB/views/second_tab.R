second_tab_content <- material_tab_content(
  tab_id = "second_tab",
  material_row(
    material_column(
      width = 12,
      material_card(
        title = "Map",
         leafletOutput("map")
      )
    )
  )
)

