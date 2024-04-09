output_datatable <- function(df,
                             region_type,
                             df_cols = list(),
                             valueformat = DT::formatCurrency) {
  col_length = length(df_cols)
  fixed_cols = col_length - 3
  
  shared_df <- df %>%
    select(unlist(df_cols)) %>%
    mutate(date = format(date, "%Y %b")) %>%
    pivot_wider(names_from = date,
                values_from = value) %>%
    create_crosstalk_input(region_type = region_type)
  
  output <- shared_df |>
    datatable(
      extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        scrollX = TRUE,
        fixedColumns = list(leftColumns = fixed_cols),
        columnDefs = list(list(
          visible = FALSE,
          targets = c(0)
        ))
      ),
      filter = "top",
      rownames = FALSE
    ) %>%
    valueformat(-(1:(fixed_cols + 1)), digits = 0)
  
  return(output)
}


dt_cols <- list(
  fm = list(
    "key_id",
    "Index Type" = "index_type",
    "Seasonal Adjustment" = "seasonal_adjustment",
    "Base Year" = "base_year",
    Location = "location_name",
    "date",
    "value"
  ),
  fhfa = list(
    "key_id",
    "Type" = "hpi_type",
    "Flavor" = "hpi_flavor",
    "Frequency" = "frequency",
    "Index Type" = "index_type",
    "Seasonal Adjustment" = "seasonal_adjustment",
    "Base Year" = "base_year",
    Location = "location_name",
    "date",
    "value"
  )
)
