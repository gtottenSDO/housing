# first download data from https://www.apartmentlist.com/research/category/data-rent-estimates
# download most recent "Historic Rent Estimates (Jan 2017 to Present)" file and 
# "Apartment List Vacancy Index (Jan 2017 to Present)" file and save each to the "_data" folder

# read in data from csv files - update file names as needed

process_apartmentlist_estimates <- function(file) {
    vroom(file) %>%
        pivot_longer(cols = -c(location_name:bed_size), names_to = "date", values_to = "rent_estimate") %>%
        filter(!is.na(rent_estimate)) %>%
        mutate(
            date = ym(date),
            bed_size = factor(bed_size, levels = c("overall", "1br", "2br"))
        ) %>% 
    filter(month(date) %in% c(3,6,9,12))  %>% 
    group_by(date, bed_size, location_type) %>%
    arrange(date, bed_size, desc(rent_estimate)) %>% 
    mutate(
      fips = str_pad(location_fips_code, 2, pad = "0"),
      rent_rank = rank(rent_estimate)) %>%
    ungroup() %>%
    group_by(metro) %>%
    mutate(
      state = ifelse(is.na(state), paste(unique(state[!is.na(state)]), collapse = ", "), state)
    ) %>%
    ungroup()

}

process_apartmentlist_vacancy <- function(file) {
    vacancy_index <- vroom(file) %>%
        pivot_longer(
            cols = -c(location_name:metro),
            names_to = "Date",
            values_to = "Vacancy_Index"
        ) %>%
        mutate(Date = ym(Date))
}


