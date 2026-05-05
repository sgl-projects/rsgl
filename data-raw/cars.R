cars <- read.csv("data-raw/cars.csv", stringsAsFactors = FALSE)
usethis::use_data(cars, overwrite = TRUE)
