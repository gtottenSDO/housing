create_fhfa_filters <- function(fhfa_df, r_lvel) {
  shared_df <-
    create_crosstalk_input(fhfa_df, region_type = r_lvel, key = ~ key_id)
  
  filters <- list()
  
  filters$date <- try(filter_slider("d_state",
                                    "Date",
                                    shared_df,
                                    ~ year(date)),
                      silent = TRUE)
  
  try(filters$hpi_type <-
        filter_select(paste("hpit_", fhfa, r_lvel, sep = "_"),
                      "Index Type",
                      shared_df,
                      ~ hpi_type,
                      multiple = FALSE),
      silent = TRUE)
  
  try(filters$hpi_flavor <- filter_select(
    paste("hpi_f", fhfa, r_lvel, sep = "_"),
    "Index Flavor",
    shared_df,
    ~ hpi_flavor,
    multiple = FALSE
  ),
  silent = TRUE)
  
  try(filters$frequency <- filter_select(
    paste("freq_", fhfa, r_lvel, sep = "_"),
    "Frequency",
    shared_df,
    ~ frequency,
    multiple = FALSE
  ),
  silent = TRUE)

  try(filters$i_type <- filter_select(
    paste("i_type_", fhfa, r_lvel, sep = "_"),
    "Index Type",
    shared_df,
    ~ index_type,
    multiple = FALSE
  ),
  silent = TRUE)

  try(filters$sa <- filter_select(
    paste("sa_", fhfa, r_lvel, sep = "_"),
    "Seasonal Adjustment",
    shared_df,
    ~ seasonal_adjustment,
    multiple = FALSE
  ),
  silent = TRUE)

  try(filters$state <- filter_select("s_state",
                                 "State",
                                 shared_df,
                                 ~ state,
                                 multiple = TRUE),
      silent = TRUE)
  
  return(filters)
  
}

# create_filter(df, region_type, filter_col, filter_label) {
#   filter_select(
#     paste(filter_col, region_type, sep = "_"),
#     filter_label,
#     df,
#     ~ filter_col,
#     multiple = TRUE
#   )
#   
#   
# }

