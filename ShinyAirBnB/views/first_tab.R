first_tab_content <- material_tab_content(
  tab_id = "first_tab",
  material_parallax(
    image_source =
      "https://besthqwallpapers.com/img/original/107273/4k-madrid-gran-via-cityscapes-spanish-cities.jpg"
  ),
  material_card(
    title = "Filters",
    material_row(
      align = "center",
      material_column(
        offset = 2,
        width = 4,
        material_date_picker(
          input_id = "start_date",
          color = "red",
          label = "Start date"
        ),
      ),
      material_column(
        width = 4,
        material_date_picker(
          input_id = "end_date",
          color = "red",
          label = "End date"
        )
      )
    )
  ),
  material_card(
    title = "Bar charts",
    material_row(
      material_column(
        width = 6,
        material_card(
          title = "Price per year",
          plotOutput("calendar_price_year_plot")
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "Number of Reviews per year",
          plotOutput("calendar_reviews_year_plot")
        )
      )
    )),
  material_card(
    title = "Sentiment Analysis",
    material_row(
      material_column(
        width = 6,
        material_card(
          title = "WordCloud based on positive reviews",
          plotOutput("sentiment_analysis_positive_wordcloud")
        )
      ),
      material_column(
        width = 6,
        material_card(
          title = "WordCloud based on negative reviews",
          plotOutput("sentiment_analysis_negative_wordcloud")
        )
      )
    )),
)

