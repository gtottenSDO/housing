# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "vroom", "plotly",
  "scales", "purrr"),
  format = "rds",
  error = "null")

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source)
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    al_rent_file, 
    "_data/Apartment_List_Rent_Estimates.csv", 
    format = "file"),
  tar_target(
    al_vacancy_file, 
    "_data/Apartment_List_Vacancy_Index.csv",
    format = "file"),
  tar_target(
    al_rents,
    process_apartmentlist_estimates(al_rent_file)
  ),
  tar_target(
    al_vacancy,
    process_apartmentlist_vacancy(al_vacancy_file)
  ),
  tar_target(
    al_rent_plots,
    al_create_plots(al_rents)
  )
)
