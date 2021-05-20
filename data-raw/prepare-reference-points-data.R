fma_reference_points <- readr::read_csv("data-raw/reference_points.csv")

usethis::use_data(
  fma_reference_points,
  overwrite = TRUE
)
