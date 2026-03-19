test_that("creates connection with correct tables", {
  # test_con is created in tests/testthat/setup.R
  df <- DBI::dbGetQuery(test_con, "show tables")
  actual_table_names <- df$name

  expected_table_names <- c(
    "cars", "diamonds", "economics", "synth"
  )
  expect_equal(actual_table_names, expected_table_names)
})
