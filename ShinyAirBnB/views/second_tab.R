first_tab_content <- material_tab_content(
  tab_id = "first_tab",
  material_parallax(
    image_source =
      "https://grupoacerta.com/wp-content/uploads/2019/07/madrid-landscape.jpg"
  ),
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

