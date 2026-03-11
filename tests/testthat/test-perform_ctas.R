normalize_df <- function(df) {
  df |>
    dplyr::ungroup() |>
    (\(x) dplyr::select(x, sort(names(x))))() |>
    dplyr::arrange(dplyr::across(dplyr::everything()))
}

expect_equal_ignore_order <- function(df_1, df_2) {
  ordered_df_1 <- normalize_df(df_1)
  ordered_df_2 <- normalize_df(df_2)
  expect_equal(ordered_df_1, ordered_df_2) # nolint: object_usage_linter
}

test_that("doesn't modify dataframe if no cta's specified", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using points
	")
  input_dfs <- result_dfs(rgs, test_con)

  actual_dfs <- perform_ctas(rgs, input_dfs)

  expect_equal(length(actual_dfs), length(input_dfs))
  Map(
    expect_equal_ignore_order,
    actual_dfs,
    input_dfs
  )
})

test_that("performs cta's for single layer", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			count(*) as y
		from cars
		group by
			bin(mpg)
		using bars
	")
  input_dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- input_dfs[[1]]
  scales <- rgs$scales

  actual_dfs <- perform_ctas(rgs, input_dfs)

  transformed_df <- perform_cts_for_layer(
    layer,
    input_df,
    scales
  )
  aggregated_df <- perform_as_for_layer(
    layer,
    transformed_df,
    scales
  )
  expected_dfs <- list(aggregated_df)

  expect_equal(length(actual_dfs), length(expected_dfs))
  Map(
    expect_equal_ignore_order,
    actual_dfs,
    expected_dfs
  )
})

test_that(
  "performs cta's for transformed layer and leaves untransformed layer as is",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points

			layer

			visualize
				bin(mpg) as x,
				count(*) as y
			from cars
			group by
				bin(mpg)
			using bars
		")
    input_dfs <- result_dfs(rgs, test_con)
    first_layer <- rgs$layers[[1]]
    first_input_df <- input_dfs[[1]]
    second_input_df <- input_dfs[[2]]
    scales <- rgs$scales

    actual_dfs <- perform_ctas(rgs, input_dfs)

    transformed_df <- perform_cts_for_layer(
      first_layer,
      first_input_df,
      scales
    )
    aggregated_df <- perform_as_for_layer(
      first_layer,
      transformed_df,
      scales
    )
    expected_dfs <- list(aggregated_df, second_input_df)

    expect_equal(length(actual_dfs), length(expected_dfs))
    Map(
      expect_equal_ignore_order,
      actual_dfs,
      expected_dfs
    )
  }
)

test_that("performs cta's for multiple transformed layers", {
  rgs <- sgl_to_rgs("
		visualize
			cyl as x,
			count(*) as y
		from cars
		group by
			cyl
		using points

		layer

		visualize
			bin(mpg) as x,
			count(*) as y
		from cars
		group by
			bin(mpg)
		using bars
	")
  input_dfs <- result_dfs(rgs, test_con)
  first_layer <- rgs$layers[[1]]
  first_input_df <- input_dfs[[1]]
  second_layer <- rgs$layers[[2]]
  second_input_df <- input_dfs[[2]]
  scales <- rgs$scales

  actual_dfs <- perform_ctas(rgs, input_dfs)

  first_transformed_df <- perform_cts_for_layer(
    first_layer,
    first_input_df,
    scales
  )
  first_aggregated_df <- perform_as_for_layer(
    first_layer,
    first_transformed_df,
    scales
  )
  second_transformed_df <- perform_cts_for_layer(
    second_layer,
    second_input_df,
    scales
  )
  second_aggregated_df <- perform_as_for_layer(
    second_layer,
    second_transformed_df,
    scales
  )

  expected_dfs <- list(first_aggregated_df, second_aggregated_df)

  expect_equal(length(actual_dfs), length(expected_dfs))
  Map(
    expect_equal_ignore_order,
    actual_dfs,
    expected_dfs
  )
})
