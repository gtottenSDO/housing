output_rent_datatable <- function(df, region_type) {

    shared_df <- df %>%
        select(
            key_id,
            Location = location_name,
            "Rental Type" = rental_type,
            date,
            rent_estimate
        ) %>%
        mutate(
            date = format(date, "%Y %b")
        ) %>%
        pivot_wider(
            names_from = date,
            values_from = rent_estimate
        ) %>%
      create_crosstalk_input(region_type = region_type)
    
    output <- shared_df |> 
      datatable(
            extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"),
            options = list(
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel", "pdf", "print"),
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 2),
                columnDefs = list(
                  list(
                    visible = FALSE,
                    targets = c(0)
                  )
                )
            ),
            filter = "top",
            rownames = FALSE
        ) %>%
        formatCurrency(-(1:3), digits = 0)
    
    return(output)
}


