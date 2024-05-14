refresh_targets <- function() {
  library(targets)
  rm(list = ls())
  tar_make()
  tar_load_globals()
  tar_load_everything()
}
