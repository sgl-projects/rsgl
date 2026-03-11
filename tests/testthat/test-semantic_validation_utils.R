test_that("column_from_aes returns correct column", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_col <- column_from_aes(layer, df, "x")
  expected_col <- df$hp
  expect_equal(actual_col, expected_col)
})

test_that(
  "valid_positional_aes doesn't raise error if both x and y are present",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points
		")

    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_positional_aes(layer)
    )
  }
)

test_that("valid_positional_aes doesn't raise error if only y is present", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as y
    from cars
    using points
  ")

  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_positional_aes(layer)
  )
})

test_that("valid_positional_aes doesn't raise error if only x is present", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x
    from cars
    using points
  ")

  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_positional_aes(layer)
  )
})

test_that(
  "valid_positional_aes doesn't raise error if both r and theta are present",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as r,
				mpg as theta
			from cars
			using points
		")

    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_positional_aes(layer)
    )
  }
)

test_that("valid_positional_aes doesn't raise error if only theta is present", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as theta
    from cars
    using points
  ")

  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_positional_aes(layer)
  )
})

test_that("valid_positional_aes doesn't raise error if only r is present", {
  rgs <- sgl_to_rgs("
    visualize
      hp as r
    from cars
    using points
  ")

  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_positional_aes(layer)
  )
})

test_that("valid_positional_aes raises error for mixed coordinate system", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as theta
    from cars
    using points
  ")

  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: found aesthetics from multiple coordinate systems.",
    "All positional aesthetics must be from a single coordinate system."
  )
  expect_error(
    valid_positional_aes(layer),
    expected_msg,
    fixed = TRUE
  )
})

test_that(
  "valid_positional_aes raises error when no positional aesthetics provided",
  {
    rgs <- sgl_to_rgs("
			visualize
				mpg as color
			from cars
			using points
		")

    layer <- rgs$layers[[1]]

    expect_error(
      valid_positional_aes(layer),
      "Error: positional aesthetics must be provided, none were found.",
      fixed = TRUE
    )
  }
)

test_that(
  "counts_are_applied_to_star doesn't raise error if counts are applied to *",
  {
    rgs <- sgl_to_rgs("
			visualize
				boolean as x,
				count(*) as y,
				count(*) as color
			from synth
			group by
				boolean
			using points
		")

    layer <- rgs$layers[[1]]

    expect_no_error(
      counts_are_applied_to_star(layer)
    )
  }
)

test_that(
  "counts_are_applied_to_star raises error if count is applied to column",
  {
    rgs <- sgl_to_rgs("
			visualize
				boolean as x,
				count(day) as y
			from synth
			group by
				boolean
			using points
		")

    layer <- rgs$layers[[1]]

    expect_error(
      counts_are_applied_to_star(layer),
      "Error: count transformations must be applied to *.",
      fixed = TRUE
    )
  }
)
