# first download data from https://www.apartmentlist.com/research/category/data-rent-estimates
# download most recent "Historic Rent Estimates (Jan 2017 to Present)" file and
# "Apartment List Vacancy Index (Jan 2017 to Present)" file and save each to the "_data" folder

# read in data from csv files - update file names as needed

process_al_estimates <- function(file) {
    al_rents <- duckplyr::read_csv_duckdb(file) %>%
        mutate(across('2017_01':last_col(), as.numeric)) %>% 
        pivot_longer(
            cols = -c(location_name:bed_size),
            names_to = "date",
            values_to = "value"
        ) %>%
        filter(!is.na(value)) %>%
        rename(rental_type = bed_size) |>
        mutate(
            key_id = paste(location_name, location_type, rental_type, sep = "_"),
            date = ym(date)
        ) %>%
        group_by(date, rental_type, location_type) %>%
        arrange(date, rental_type, desc(value)) %>%
        mutate(
            fips = str_pad(location_fips_code, 2, pad = "0"),
            rank = rank(desc(value))
        ) %>%
        ungroup() %>%
        group_by(metro) %>%
        mutate(
            state = ifelse(is.na(state),
                paste(unique(state[!is.na(state)]),
                    collapse = ", "
                ),
                state
            ),
            co_flag = ifelse(state == "Colorado", TRUE, FALSE)
        ) %>%
        ungroup() %>% 
      as_duckdb_tibble()
    
    return(al_rents)
}


process_al_vacancy <- function(file) {
    vacancy_index <- duckplyr::read_csv_duckdb(file) %>%
      mutate(across('2017_01':last_col(), as.numeric)) %>% 
        pivot_longer(
            cols = -c(location_name:metro),
            names_to = "Date",
            values_to = "Vacancy_Index"
        ) %>%
        mutate(Date = ym(Date)) |>
        SharedData$new()
}

process_zori_estimates <-
    function(file,
             rental_type = c("All Homes", "Single Family", "Multi Family")) {
      zori_est <- duckplyr::read_csv_duckdb(file) %>%
            select(-c(RegionID, SizeRank)) %>%
            mutate(rental_type = rental_type, .before = 1) %>%
            rename(
                location_name = RegionName,
                location_type = RegionType,
                state = StateName
            ) %>%
            pivot_longer(
                cols = -c(rental_type:state),
                names_to = "date",
                values_to = "value"
            ) |>
            mutate(
                key_id = paste(location_name, location_type, rental_type, sep = "_"),
                date = ymd(date)) |>
            group_by(date, rental_type, location_type) |>
            arrange(date, rental_type, desc(value)) |>
            mutate(
                rank = rank(desc(value)),
                co_flag = ifelse(state == "CO",
                    TRUE,
                    FALSE
                )
            ) |>
            ungroup() %>% 
        as_duckdb_tibble()
      
      
    }

combine_zori_estimates <-
    function(all_homes_file,
             single_family_file,
             multi_family_file) {
        process_zori_estimates(all_homes_file, "All Homes") %>%
            bind_rows(process_zori_estimates(single_family_file, "Single Family")) %>%
            bind_rows(process_zori_estimates(multi_family_file, "Multi Family"))
    }
