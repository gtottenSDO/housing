create_crosstalk_input <- function(df, region_type, ...) {
  
    group_name <- paste0(deparse(substitute(df)), "_", region_type)
    
    if(exists("location_name", where = as.environment(df))) {
        df <- df %>%
            filter(location_type == region_type)
    } 
    
    SharedData$new(df, group = group_name, ...) 

}

create_rent_outputs <- function(df) {
  location_types <- split(df, df$location_type)
  output <- list()

  output$plots <- imap(location_types, ~output_rent_plot(.x, .y))
  
  output$datatables <- imap(location_types, ~output_rent_datatable(.x, .y))
  
  output$filters <- imap(location_types, ~create_rent_filters(.x, .y))
    
  return(output)
}

create_rent_filters <- function(df, region_type) {
  shared_df <- create_crosstalk_input(df, region_type = region_type, key = ~key_id)
  
  filters <- list()
  
  filters$date <- filter_slider(
    paste0("d_", region_type),
    "Date",
    shared_df,
    ~year(date)
  )
  
  filters$state <- filter_select(
    paste0("s_", region_type),
    "State",
    shared_df,
    ~state,
    multiple = TRUE
  )
  
  filters$rent_type <- filter_select(
    id = paste0("rt_", region_type),
    label = "Rental Type",
    shared_df,
    ~rental_type,
    multiple = FALSE
  )
  
  return(filters)
  
}