test_that("returns correct dataframe for table name source", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
  ")

  actual_dfs <- result_dfs(rgs, test_con)

  expected_df <- DBI::dbGetQuery(test_con, "select * from cars")
  expect_equal(actual_dfs[[1]], expected_df)
})

test_that("returns correct dataframe for subquery source", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from (
			select *
			from cars
			where cyl = 4
		)
    using points
  ")

  actual_dfs <- result_dfs(rgs, test_con)

  expected_df <- DBI::dbGetQuery(test_con, "
		select *
		from cars
		where cyl = 4
	")
  expect_equal(actual_dfs[[1]], expected_df)
})

test_that("raises error if table doesnt exist", {
  rgs <- sgl_to_rgs("
    visualize
      cty as x,
      hwy as y
    from not_cars
    using points
  ")

  expect_error(
    result_dfs(rgs, test_con),
    "Table with name not_cars does not exist",
    fixed = TRUE
  )
})

test_that("raises error for invalid subquery", {
  rgs <- sgl_to_rgs("
    visualize
      cty as x,
      hwy as y
    from (select something)
    using points
  ")

  expected_msg <- paste(
    'Referenced column "something" was not found',
    "because the FROM clause is missing"
  )
  expect_error(
    result_dfs(rgs, test_con),
    expected_msg,
    fixed = TRUE
  )
})

test_that("returns single df for single layer", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
  ")

  actual_dfs <- result_dfs(rgs, test_con)

  expect_equal(length(actual_dfs), 1)
})

test_that("returns multiple dfs for multiple layers", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points

		layer

    visualize
      hp as x,
      mpg as y
    from (
			select *
			from cars
			where cyl = 4
		)
    using points
  ")

  actual_dfs <- result_dfs(rgs, test_con)

  expected_df_1 <- DBI::dbGetQuery(test_con, "select * from cars")
  expected_df_2 <- DBI::dbGetQuery(test_con, "
		select *
		from cars
		where cyl = 4
	")
  expect_equal(length(actual_dfs), 2)
  expect_equal(actual_dfs[[1]], expected_df_1)
  expect_equal(actual_dfs[[2]], expected_df_2)
})
