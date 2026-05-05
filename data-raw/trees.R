trees <- read.csv("data-raw/trees.csv", stringsAsFactors = FALSE)
usethis::use_data(trees, overwrite = TRUE)
