test_that("doesn't add columns if none are transformed", {
  rgs <- sgl_to_rgs("
    visualize
			cut as x,
			count(*) as y
    from diamonds
		group by
			cut, color
		collect by
			color
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)
  expect_equal(result_df, input_df)
})

test_that("adds transformed column for linear scaled aes", {
  rgs <- sgl_to_rgs("
    visualize
			bin(mpg) as x
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    input_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})

test_that("adds transformed column for log scaled aes", {
  rgs <- sgl_to_rgs("
    visualize
			bin(mpg) as x
    from cars
    using points
		scale by
			log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    input_df,
    scale = new_sgl_scale_log()
  )

  expect_equal(result_df, expected_df)
})

test_that("adds transformed columns for both linear and log scaled aes", {
  rgs <- sgl_to_rgs("
    visualize
			bin(mpg) as x,
			bin(hp) as y
    from cars
    using points
		scale by
			log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  int_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "hp",
    input_df,
    scale = new_sgl_scale_linear()
  )
  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    int_df,
    scale = new_sgl_scale_log()
  )

  expect_equal(result_df, expected_df)
})

test_that("doesn't add duplicate transformed column from aes", {
  rgs <- sgl_to_rgs("
    visualize
			bin(mpg) as x,
			bin(mpg) as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    input_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})

test_that("adds transformed column for grouping", {
  rgs <- sgl_to_rgs("
    visualize
			count(*) as x
    from cars
		group by
			bin(mpg)
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    input_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})

test_that("adds multiple transformed columns for groupings", {
  rgs <- sgl_to_rgs("
    visualize
			count(*) as x
    from cars
		group by
			bin(mpg),
			bin(hp)
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  int_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "hp",
    input_df,
    scale = new_sgl_scale_linear()
  )
  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    int_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})

test_that("adds transformed column for collection", {
  rgs <- sgl_to_rgs("
    visualize
			disp as x,
			hp as y
    from cars
		collect by
			bin(mpg)
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    input_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})

test_that("adds multiple transformed columns for collections", {
  rgs <- sgl_to_rgs("
    visualize
			disp as x,
			hp as y
    from cars
		collect by
			bin(mpg),
			bin(wt)
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  int_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "wt",
    input_df,
    scale = new_sgl_scale_linear()
  )
  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    int_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})

test_that("doesn't add duplicate transformed columns across clauses", {
  rgs <- sgl_to_rgs("
    visualize
			bin(mpg) as x,
			bin(hp) as y,
			count(*) as color
    from cars
		group by
			bin(mpg),
			bin(hp)
		collect by
			bin(hp)
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_cts_for_layer(layer, input_df, scales)

  int_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "hp",
    input_df,
    scale = new_sgl_scale_linear()
  )
  expected_df <- add_transformed_column(
    new_sgl_cta_bin(),
    "mpg",
    int_df,
    scale = new_sgl_scale_linear()
  )

  expect_equal(result_df, expected_df)
})
