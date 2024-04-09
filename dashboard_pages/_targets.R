# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(crosstalk)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse", "vroom", "plotly",
    "scales", "DT"
  ),
  format = "rds",
  error = "null"
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source)

# Replace the target list below with your own:
list(
  tar_target(
    al_rent_file,
    "_data/Apartment_List_Rent_Estimates.csv",
    format = "file"
  ),
  tar_target(
    al_vacancy_file,
    "_data/Apartment_List_Vacancy_Index.csv",
    format = "file"
  ),
  tar_target(
    al_rents,
    process_al_estimates(al_rent_file)
  ),
  tar_target(
    al_vacancy,
    process_al_vacancy(al_vacancy_file)
  ),
  tar_target(
    al_outputs,
    create_rent_outputs(al_rents)
  ),
  tar_target(
    zori_all_homes_file,
    "_data/Metro_zori_uc_sfrcondomfr_sm_month.csv",
    format = "file"
  ),
  tar_target(
    zori_single_family_file,
    "_data/Metro_zori_uc_sfr_sm_month.csv",
    format = "file"
  ),
  tar_target(
    zori_multi_family_file,
    "_data/Metro_zori_uc_mfr_sm_month.csv",
    format = "file"
  ),
  tar_target(
    zori_rents,
    combine_zori_estimates(
      zori_all_homes_file,
      zori_single_family_file,
      zori_multi_family_file
    )
  ),
  tar_target(
    zori_outputs,
    create_rent_outputs(zori_rents)
  ),
  tar_target(
    fhfa_file,
    "_data/HPI_master.csv",
    format = "file"
  ),
  tar_target(
    fhfa_raw,
    process_fhfa_hpi(fhfa_file)
  ),
  tar_target(
    fhfa_hpi,
    process_hpi(fhfa_raw)
  ),
  tar_target(
    fm_file,
    "_data/fmhpi_master_file.csv",
    format = "file"
  ),
  tar_target(
    fm_raw,
    process_fm_hpi(fm_file)
  ),
  tar_target(
    fm_hpi,
    process_hpi(fm_raw)
  )
)
