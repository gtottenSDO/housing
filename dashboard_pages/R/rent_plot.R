plot_rents <- function(rent_df) {

  plot_ly(rent_df) %>%
    group_by(key_id) %>%
    add_lines(x = ~date, y = ~rent_estimate, 
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
                scales::dollar(rent_estimate),
                "<br>Rank: ",
                rent_rank
              )) %>%
    layout(xaxis = list(title = "")) %>%
    layout(
      xaxis = list(title = "Date"),
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

plot_ranks <- function(rent_df) {

  plot_ly(rent_df) %>%
    filter(co_flag ==1) |> 
    group_by(key_id) %>%
    add_lines(x = ~date, y = ~rent_rank, 
              hoverinfo = "text",
              hovertext = ~ paste(
                "Date: ",
                date,
                "<br>",
                location_type,
                ": ",
                location_name,
                "<br>Rent: ",
                scales::dollar(rent_estimate),
                "<br>Rank: ",
                rent_rank
              )) %>%
    layout(
      xaxis = list(
        title = ""),
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

output_rent <- function(shared_df) {

  subplot(plot_rents(shared_df), plot_ranks(shared_df),
          nrows = 2, shareX = TRUE,
          heights = c(0.7, 0.3)) |> 
    highlight()
    

}

