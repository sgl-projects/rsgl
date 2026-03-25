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
