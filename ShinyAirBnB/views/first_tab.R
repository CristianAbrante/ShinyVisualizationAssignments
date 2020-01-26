first_tab_content <- material_tab_content(
  tab_id = "first_tab",
  material_parallax(
    image_source =
      "https://grupoacerta.com/wp-content/uploads/2019/07/madrid-landscape.jpg"
  ),
  material_row(
    material_column(
      width = 3,
      material_slider(
        input_id = "from_year",
        label = "From Year",
        min_value = min(get_year_from_date(as.vector(calendar$date))),
        max_value = max(get_year_from_date(as.vector(calendar$date))),
        initial_value = median(get_year_from_date(as.vector(calendar$date))),
        color = "blue"
      )
    ),
    material_column(
      width = 3,
      material_slider(
        input_id = "to_year",
        label = "To Year",
        min_value = min(get_year_from_date(as.vector(calendar$date))),
        max_value = max(get_year_from_date(as.vector(calendar$date))),
        initial_value = median(get_year_from_date(as.vector(calendar$date))),
        color = "blue"
      )
    )
  ),
  material_row(
    material_column(
      width = 12,
      material_card(
        title = "Price per year",
        plotOutput("calendar_price_year"),
        uiOutput("calendar_price_year")
      )
    )
  )
)

