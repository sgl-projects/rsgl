create_con_and_load_data <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "cars", datasets::mtcars)
  DBI::dbWriteTable(con, "economics", as.data.frame(ggplot2::economics))
  DBI::dbWriteTable(
    con, "synth",
    data.frame(
      letter = rep(letters[1:3], 2),
      number = 1:6,
      day = seq(as.Date("1950-01-01"), length.out = 6, by = "10 years"),
      day_and_time = seq(
        as.POSIXct("1950-01-01"),
        length.out = 6, by = "10 years"
      ),
      boolean = c(rep(TRUE, 3), rep(FALSE, 3))
    )
  )
  diamonds <- ggplot2::diamonds
  categorical_cols <- c("cut", "color", "clarity")
  for (col in categorical_cols) {
    diamonds[[col]] <- as.character(diamonds[[col]])
  }
  DBI::dbWriteTable(con, "diamonds", diamonds)
  con
}
