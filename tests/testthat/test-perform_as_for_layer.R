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

test_that("doesn't do anything if no aggregations are specified", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using points
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_as_for_layer(
    layer, input_df, scales
  )

  expect_equal_ignore_order(result_df, input_df)
})

test_that("returns only count col if all mappings are counts", {
  rgs <- sgl_to_rgs("
		visualize
			count(*) as x,
			count(*) as color
		from cars
		using points
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_as_for_layer(
    layer, input_df, scales
  )

  expected_df <- DBI::dbGetQuery(
    test_con,
    "select count(*) as 'rsgl.count' from cars"
  )
  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns untransformed col and count", {
  rgs <- sgl_to_rgs("
		visualize
			vs_cat as x,
			count(*) as y
		from (
			select
				*,
				cast(vs as varchar) as vs_cat
			from cars
		)
		group by
			vs_cat
		using bars
	")
  dfs <- result_dfs(rgs, test_con)
  input_df <- dfs[[1]]
  layer <- rgs$layers[[1]]
  scales <- rgs$scales

  result_df <- perform_as_for_layer(
    layer, input_df, scales
  )

  expected_df <- input_df |>
    dplyr::group_by(vs_cat) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned col and count", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			count(*) as y
		from cars
		group by
			bin(mpg)
		using bars
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_linear()
  )

  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(rsgl.linear.bin.mpg) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns log scaled binned col and count", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			count(*) as y
		from cars
		group by
			bin(mpg)
		using bars
		scale by
			log(x)
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_log()
  )

  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(rsgl.log.bin.mpg) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned cols and count for multiple scales", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			bin(mpg) as y,
			count(*) as color
		from cars
		group by
			bin(mpg)
		using bars
		scale by
			log(y)
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  int_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_log()
  )
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", int_df,
    scale = new_sgl_scale_linear()
  )


  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(rsgl.log.bin.mpg, rsgl.linear.bin.mpg) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned and unbinned col and count", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			count(*) as y,
			vs_cat as color
		from (
			select
				*,
				cast(vs as varchar) as vs_cat
			from cars
		)
		group by
			bin(mpg),
			vs_cat
		using bars
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_linear()
  )

  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(rsgl.linear.bin.mpg, vs_cat) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns additional grouping not in aes mapping", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			count(*) as y
		from (
			select
				*,
				cast(vs as varchar) as vs_cat
			from cars
		)
		group by
			bin(mpg),
			vs_cat
		using bars
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_linear()
  )

  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(rsgl.linear.bin.mpg, vs_cat) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns additional binned grouping not in aes mapping", {
  rgs <- sgl_to_rgs("
		visualize
			vs_cat as x,
			count(*) as y
		from (
			select
				*,
				cast(vs as varchar) as vs_cat
			from cars
		)
		group by
			vs_cat,
			bin(mpg)
		using bars
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_linear()
  )

  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(vs_cat, rsgl.linear.bin.mpg) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})
