test_con <- create_con_and_load_data()

withr::defer(
  DBI::dbDisconnect(test_con, shutdown = TRUE),
  teardown_env()
)
