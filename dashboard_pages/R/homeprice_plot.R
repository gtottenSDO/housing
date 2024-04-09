plot_value <- function(df, 
                       y_label, 
                       y_currency = TRUE) {
  if (y_currency) {
    y_format <- scales::dollar
    y_tick <- "$,.0f"
  } else {
    y_format <- scales::comma
    y_tick <- ",.0f"
  }
  
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
        "<br>", y_label, ": ",
        y_format(value),
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
        title = y_label,
        tickformat = y_tick,
        side = "left"
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, t = 50, b = 50),
      template = "plotly_dark"
    )
}


plot_ranks <- function(df,
                       y_label, 
                       y_currency = TRUE) {
  if (y_currency) {
    y_format <- scales::dollar
    y_tick <- "$,.0f"
  } else {
    y_format <- scales::comma
    y_tick <- ",.0f"
  }
  
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
        "<br>", y_label, ": ",
        y_format(value),
        "<br>Rank: ",
        rank
      ))  |> 
    layout(
      yaxis = list(
        title = "Rank",
        side = "left",
        autorange = "reversed"
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, t = 50, b = 50),
      template = "plotly_dark"
    )
}

output_plot <- function(df, region_type, 
                             start_date = NULL,
                             end_date = NULL,
                             y_label,
                             y_currency = TRUE) {
  df <- df |> 
    filter(!is.na(value))

   
  shared_df <- create_crosstalk_input(df, region_type = region_type, key = ~key_id)
  
  
  
  p <- subplot(
    plot_value(shared_df, y_label = y_label, y_currency = y_currency),
    plot_ranks(shared_df, y_label = y_label, y_currency = y_currency),
    nrows = 2, shareX = TRUE,
    titleY = TRUE,
    heights = c(0.7, 0.3)
  ) |>
    highlight()
  
  p

}

filter_dates <- function(df, start_date = NULL, end_date = NULL) {
  if(!is.null(start_date)) {
    df <- df |> 
      filter(is.null(start_date) | date >= start_date)
  }
  if(!is.null(end_date)) {
    df <- df |> 
      filter(is.null(end_date) | date <= end_date)
  }
  return(df)
}
