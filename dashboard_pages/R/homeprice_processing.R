process_hpi <- function(hpi_df) {
  hpi_ranked <- hpi_df |>
    group_by(across(c(-value, -date))) |>
    mutate(key_id = cur_group_id()) |>
    group_by(date, .add = TRUE) |>
    ungroup(location_name) |>
    mutate(base_year = "default",
           rank = rank(desc(value), ties.method = "min")) |>
    ungroup() |> 
    as_duckdb_tibble()
  
  hpi_reindexed <- reindex_prices(hpi_ranked) |> 
    as_duckdb_tibble()
   
  bind_rows(hpi_ranked,
            hpi_reindexed) |> 
    select(-base_value) |> 
    group_by(across(c(-value, -date, - rank))) |> 
    mutate(key_id = cur_group_id()) |>
    ungroup() |> 
    as_duckdb_tibble()
}

process_fhfa_hpi <- function(file) {
  fhfa_hpi <- read_csv_duckdb(file, prudence = "lavish") |>
    mutate(date = as.Date(ifelse(
      frequency == "quarterly",
      yq(paste0(yr, "-", period)),
      ym(paste0(yr, "-", period))
    ))) |>
    select(-yr, -period, -place_id) |>
    rename(
      location_name = place_name,
      location_type = level,
      "Index_Seasonally Adjusted" = index_sa,
      "Index_Not Seasonally Adjusted" = index_nsa
    ) |>
    pivot_longer(
      cols = c("Index_Seasonally Adjusted",
               "Index_Not Seasonally Adjusted"),
      names_to = c("index_type", "seasonal_adjustment"),
      names_sep = "_",
      values_to = "value"
    ) |> 
    as_duckdb_tibble()
  
}

process_fm_hpi <- function(file) {
  fm_hpi <- read_csv_duckdb(file, prudence = "lavish") |>
    mutate(date = ym(paste0(Year, "-", Month))) |>
    select(
      location_type = GEO_Type,
      location_name = GEO_Name,
      date,
      "Index_Seasonally Adjusted" = Index_SA,
      "Index_Not Seasonally Adjusted" = Index_NSA
    ) |>
    mutate(co_flag = ifelse(
      location_type == "State",
      ifelse(location_name == "CO", TRUE, FALSE),
      ifelse(location_type == "CBSA",
             ifelse(str_sub(location_name, -2, -1) == "CO", TRUE,FALSE),
             FALSE))) |>
    pivot_longer(
      cols = c("Index_Seasonally Adjusted", "Index_Not Seasonally Adjusted"),
      names_to = c("index_type", "seasonal_adjustment"),
      names_sep = "_",
      values_to = "value"
    ) |> 
    as_duckdb_tibble()
  
}

reindex_prices <- function(df) {
  base_year_vec <- ymd(paste0(seq(2000, 2020, by = 5), "-01-01"))
  
  reindex_b_year <- function(df, b_date) {
    df <- df %>%
      group_by(key_id) %>%
      mutate(base_year = as.character(year(b_date)),
             base_value = ifelse(length(value[date == b_date]) > 0, 
                                 value[date == b_date], NA),
             value = value / base_value * 100) |> 
      ungroup() |> 
      as_duckdb_tibble()
    
  }
  
  base_year_vec |> 
    map_dfr(~reindex_b_year(df, .x))
  
}
