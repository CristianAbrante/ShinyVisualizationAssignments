third_tab_content <- material_tab_content(
  tab_id = "third_tab",
  material_row(
    material_column(
      width = 4,
      material_date_picker(
        input_id = "price_start_date",
        color = "red",
        label = "Start date"
      ),
      textOutput("price_start_date")
    ),
    material_column(
      width = 4,
      material_date_picker(
        input_id = "price_end_date",
        color = "red",
        label = "End date"
      )
    )
  ),
  material_row(
    material_column(
      width = 12,
      material_card(
        title = "Map",
        leafletOutput("cloropleth")
      )
    )
  )
)

