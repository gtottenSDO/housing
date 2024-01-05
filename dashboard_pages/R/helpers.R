refresh_targets <- function() {
  rm(list = ls())
  tar_make()
  tar_load_globals()
  tar_load_everything()
}

shades_of_blue <- function(n) {
  scales::seq_gradient_pal(low = "#0000FF", high = "#87CEEB")(n)
}
