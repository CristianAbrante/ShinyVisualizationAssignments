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
        min_value = min(calendar$date_year),
        max_value = max(calendar$date_year),
        initial_value = min(calendar$date_year),
        color = "blue"
      )
    ),
    material_column(
      width = 3,
      material_slider(
        input_id = "to_year",
        label = "To Year",
        min_value = min(calendar$date_year),
        max_value = max(calendar$date_year),
        initial_value = max(calendar$date_year),
        color = "blue"
      )
    )
  ),
  material_row(
    material_column(
      width = 12,
      material_card(
        title = "Price per year",
        plotOutput("calendar_price_year_plot"),
        uiOutput("calendar_price_year_plot_error")
      )
    )
  )
)

