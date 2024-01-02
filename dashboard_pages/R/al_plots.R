al_filter_rents <- function(al_rents, l_type, l_name, b_size = NULL) {
  if (is.null(b_size)) {
    al_rents_filtered <- al_rents |> 
      filter(location_type == l_type) |> 
      mutate(
        location_flag = if_else(location_name == l_name, TRUE, FALSE)
      )
  } else
  al_rents_filtered <- al_rents |> 
    filter(location_type == l_type & bed_size == b_size) |> 
    mutate(
      location_flag = if_else(location_name == l_name, TRUE, FALSE)
    )
}


al_rents_compare_plotly <- function(al_rents, location_type, location_name, b_size = NULL) {
  al_rents_filtered <- al_filter_rents(al_rents, location_type, location_name, b_size)
  
  plot_ly() |> 
    add_trace(data = al_rents_filtered, 
              type = "scatter",
              mode = "lines",
              x = ~date,
              y = ~rent_estimate,
              color = ~location_flag,
              colors = c("grey", "blue"),
              hoverinfo = "text",
              hovertext = ~paste(
                "Date: ", date,
                "<br>", location_type,": ", location_name,
                "<br>Rent: ", scales::dollar(rent_estimate), 
                "<br>Rank: ", rent_rank
              ),
              line = list(group = ~location_name),
              split = ~fips
    ) |> 
    add_trace(data = al_rents_filtered |> 
                filter(location_flag == TRUE),
              x = ~ date, 
              y = ~ rent_rank, 
              yaxis = "y2", 
              type = "bar",
              opacity = .25,
              hoverinfo = "skip") |>
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(
        title = "Rent Estimate",
        tickformat = "$",
        side = "left"
      ),
      yaxis2 = list(
        title = "Rent Rank",
        overlaying = "y",
        side = "right",
        range = c(0, 51)
      ),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, t = 50, b = 50),
      template = "plotly_dark"
    )
}

al_colorado_areas <- function(al_rents) {
  al_rents |> 
    filter(state == "Colorado") |> 
    select(location_name, location_type) |> 
    filter(!is.na(location_name)) |> 
    distinct()
}

al_plot_metro_rents <- function(al_rents) {
  metros <- al_colorado_areas(al_rents) |> 
  filter(location_type == "Metro") |> 
  pull(location_name)
  purrr::map(metros, ~al_rents_compare_plotly(al_rents, location_type = "Metro", location_name = .x, b_size = "overall")) |> 
    purrr::set_names(metros)
}

al_create_plots <- function(al_rents) {
  c(list(
    Colorado = al_rents_compare_plotly(al_rents, location_type = "State", location_name = "Colorado", b_size = "overall")),
    al_plot_metro_rents(al_rents)
  )
}
