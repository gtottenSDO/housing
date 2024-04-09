plot_rents <- function(df) {

    plot_ly(df) %>%
    group_by(key_id) %>%
    add_lines(
      x = ~date, y = ~value,
      color = ~co_flag, colors = c("grey", "blue"),
      hoverinfo = "text",
      hovertext = ~ paste(
        "Date: ",
        date,
        "<br>",
        location_type,
        ": ",
        location_name,
        "<br>Rent: ",
        scales::dollar(value),
        "<br>Rank: ",
        rank
      )
    ) %>%
    layout(xaxis = list(title = "")) %>%
    layout(
      xaxis = list(
        title = "Date",
        rangeslider = list(visible = T),
        rangeselector = list(
          buttons = list(
            list(count = 6, label = "6m", step = "month", stepmode = "backward"),
            list(count = 1, label = "1y", step = "year", stepmode = "backward"),
            list(count = 2, label = "2y", step = "year", stepmode = "backward"),
            list(step = "all")
          )
        )
      ),
      yaxis = list(
        title = "Rent Estimate",
        tickformat = "$",
        side = "left"
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, t = 50, b = 50),
      template = "plotly_dark"
    )
}


plot_ranks <- function(df) {

    plot_ly(df) %>%
    filter(co_flag == 1) |>
    group_by(key_id) %>%
    add_lines(
      x = ~date, y = ~rank,
      hoverinfo = "text",
      hovertext = ~ paste(
        "Date: ",
        date,
        "<br>",
        location_type,
        ": ",
        location_name,
        "<br>Rent: ",
        scales::dollar(value),
        "<br>Rank: ",
        rank
      )
    ) %>%
    layout(
      yaxis = list(
        title = "Rent Rank",
        side = "left",
        autorange = "reversed"
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, t = 50, b = 50),
      template = "plotly_dark"
    )
}

output_rent_plot <- function(df, region_type) {
  df <- df |> 
    filter(!is.na(value))
  shared_df <- create_crosstalk_input(df, region_type = region_type, key = ~key_id)
  

  
  p <- subplot(
    plot_rents(shared_df),
    plot_ranks(shared_df),
    nrows = 2, shareX = TRUE,
    titleY = TRUE,
    heights = c(0.7, 0.3)
  ) |>
    highlight()
  
  p
  
  # bscols(
  #   list(
  #   rent_type_filter,
  #   state_filter
  #   ),
  #   p,
  #   widths = c(1, 10)
  # )
}
