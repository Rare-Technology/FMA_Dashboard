## code to prepare `script` dataset goes here
library(readxl)
script <- read_excel("../data/fma_script.xlsx")
usethis::use_data(script, overwrite = TRUE)
