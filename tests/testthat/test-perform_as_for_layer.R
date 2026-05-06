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

describe("add_scaled_cols", {
  describe("no scale by clause is present", {
    it("returns original dataframe", {
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
      scales <- NULL

      result_df <- add_scaled_cols(layer, scales, input_df)

      expect_equal(result_df, input_df)
    })
  })
  describe("scale by clause is present", {
    describe("no scaled aes has avg mapping", {
      it("returns original dataframe", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						count(*) as y
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

        result_df <- add_scaled_cols(layer, scales, input_df)

        expect_equal(result_df, input_df)
      })
    })
    describe("one scaled aes has avg mapping", {
      it("adds scaled column", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						avg(hp) as y
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

        result_df <- add_scaled_cols(layer, scales, input_df)

        expected_df <- input_df |>
          dplyr::mutate(rsgl.log.hp = log10(hp))
        expect_equal(result_df, expected_df)
      })
    })
    describe("multiple scaled aes have avg mapping", {
      describe("single scale type with distinct avg mapping", {
        it("adds scaled columns", {
          rgs <- sgl_to_rgs("
						visualize
							bin(mpg) as x,
							avg(hp) as y,
							avg(cyl) as color
						from cars
						group by
							bin(mpg)
						using bars

						scale by
							log(y),
							log(color)
					")
          dfs <- result_dfs(rgs, test_con)
          layer <- rgs$layers[[1]]
          input_df <- dfs[[1]]
          scales <- rgs$scales

          result_df <- add_scaled_cols(layer, scales, input_df)

          expected_df <- input_df |>
            dplyr::mutate(
              rsgl.log.hp = log10(hp),
              rsgl.log.cyl = log10(cyl)
            )
          expect_equal_ignore_order(result_df, expected_df)
        })
      })
      describe("single scale type with same avg mapping", {
        it("doesn't duplicate scaled column", {
          rgs <- sgl_to_rgs("
						visualize
							bin(mpg) as x,
							avg(hp) as y,
							avg(hp) as color
						from cars
						group by
							bin(mpg)
						using bars

						scale by
							log(y),
							log(color)
					")
          dfs <- result_dfs(rgs, test_con)
          layer <- rgs$layers[[1]]
          input_df <- dfs[[1]]
          scales <- rgs$scales

          result_df <- add_scaled_cols(layer, scales, input_df)

          expected_df <- input_df |>
            dplyr::mutate(rsgl.log.hp = log10(hp))
          expect_equal_ignore_order(result_df, expected_df)
        })
      })
      describe("multiple scale types with avg mappings", {
        it("adds scaled columns for each without duplication", {
          rgs <- sgl_to_rgs("
						visualize
							bin(mpg) as x,
							avg(hp) as y,
							avg(hp) as color,
							avg(hp) as size
						from cars
						group by
							bin(mpg)
						using points

						scale by
							log(y),
							log(color),
							ln(size)
					")
          dfs <- result_dfs(rgs, test_con)
          layer <- rgs$layers[[1]]
          input_df <- dfs[[1]]
          scales <- rgs$scales

          result_df <- add_scaled_cols(layer, scales, input_df)

          expected_df <- input_df |>
            dplyr::mutate(
              rsgl.log.hp = log10(hp),
              rsgl.ln.hp = log(hp)
            )
          expect_equal_ignore_order(result_df, expected_df)
        })
      })
    })
  })
})

describe("summarize_args", {
  describe("aggs in aes mappings only", {
    it("adds count expr without specifying particular scale", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					count(*) as y
				from cars
				group by
					bin(mpg)
				using bars

				scale by
					log(y)
			")
      layer <- rgs$layers[[1]]
      scales <- rgs$scales

      result <- summarize_args(layer, scales)

      expected <- list(
        rsgl.count = rlang::expr(dplyr::n())
      )
      expect_equal(result, expected)
    })
    it("adds avg expr with default scale", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					avg(hp) as y
				from cars
				group by
					bin(mpg)
				using bars
			")
      layer <- rgs$layers[[1]]
      scales <- rgs$scales

      result <- summarize_args(layer, scales)

      expected <- list(
        rsgl.linear.avg.hp = rlang::expr(mean(hp))
      )
      expect_equal(result, expected)
    })
    it("adds avg expr with non-default scale", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					avg(hp) as y
				from cars
				group by
					bin(mpg)
				using bars

				scale by
					log(y)
			")
      layer <- rgs$layers[[1]]
      scales <- rgs$scales

      result <- summarize_args(layer, scales)

      expected <- list(
        rsgl.log.avg.hp = rlang::expr(mean(rsgl.log.hp))
      )
      expect_equal(result, expected)
    })
    it("adds exprs for multiple aggs", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					avg(hp) as y,
					avg(cyl) as color,
					count(*) as size
				from cars
				group by
					bin(mpg)
				using points

				scale by
					log(y)
			")
      layer <- rgs$layers[[1]]
      scales <- rgs$scales

      result <- summarize_args(layer, scales)
      sorted_result <- result[sort(names(result))]

      expected <- list(
        rsgl.log.avg.hp = rlang::expr(mean(rsgl.log.hp)),
        rsgl.linear.avg.cyl = rlang::expr(mean(cyl)),
        rsgl.count = rlang::expr(dplyr::n())
      )
      sorted_expected <- expected[sort(names(expected))]
      expect_equal(sorted_result, sorted_expected)
    })
  })
  describe("aggs in collect by clause only", {
    it("adds exprs correctly", {
      rgs <- sgl_to_rgs("
				visualize
					vs as x,
					am as y
				from cars
				group by
					vs,
					am
				collect by
					count(*),
					avg(hp),
					avg(mpg)
				using points
			")
      layer <- rgs$layers[[1]]
      scales <- rgs$scales

      result <- summarize_args(layer, scales)
      sorted_result <- result[sort(names(result))]

      expected <- list(
        rsgl.linear.avg.hp = rlang::expr(mean(hp)),
        rsgl.linear.avg.mpg = rlang::expr(mean(mpg)),
        rsgl.count = rlang::expr(dplyr::n())
      )
      sorted_expected <- expected[sort(names(expected))]
      expect_equal(sorted_result, sorted_expected)
    })
  })
  describe("aggs in aes mapping and collect by clause", {
    it("adds exprs without duplication", {
      rgs <- sgl_to_rgs("
				visualize
					vs as x,
					am as y,
					avg(hp) as color
				from cars
				group by
					vs,
					am
				collect by
					count(*),
					avg(hp),
					avg(mpg)
				using lines

				scale by
					log(color)
			")
      layer <- rgs$layers[[1]]
      scales <- rgs$scales

      result <- summarize_args(layer, scales)
      sorted_result <- result[sort(names(result))]

      expected <- list(
        rsgl.log.avg.hp = rlang::expr(mean(rsgl.log.hp)),
        rsgl.linear.avg.mpg = rlang::expr(mean(mpg)),
        rsgl.count = rlang::expr(dplyr::n())
      )
      sorted_expected <- expected[sort(names(expected))]
      expect_equal(sorted_result, sorted_expected)
    })
  })
})

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

test_that("returns correct cols for no grouping and all aggs", {
  rgs <- sgl_to_rgs("
		visualize
			count(*) as x
		from cars
		collect by
			avg(mpg)
		using lines
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_as_for_layer(
    layer, input_df, scales
  )

  expected_df <- input_df |>
    dplyr::summarize(
      rsgl.count = dplyr::n(),
      rsgl.linear.avg.mpg = mean(mpg)
    )
  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns untransformed col and aggs", {
  rgs <- sgl_to_rgs("
		visualize
			vs_cat as x,
			avg(mpg) as y,
			count(*) as color
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
    dplyr::summarize(
      rsgl.linear.avg.mpg = mean(mpg),
      rsgl.count = dplyr::n()
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned col and aggs", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			avg(hp) as y,
			count(*) as color
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
    dplyr::group_by(rsgl.linear.bin.30.mpg) |>
    dplyr::summarize(
      rsgl.linear.avg.hp = mean(hp),
      rsgl.count = dplyr::n()
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned col with arg and aggs", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg, 5) as x,
			avg(hp) as y,
			count(*) as color
		from cars
		group by
			bin(mpg, 5)
		using bars
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    num_bins = 5,
    scale = new_sgl_scale_linear()
  )

  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(rsgl.linear.bin.5.mpg) |>
    dplyr::summarize(
      rsgl.linear.avg.hp = mean(hp),
      rsgl.count = dplyr::n()
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns non-linear scaled binned col and aggs", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			avg(hp) as y,
			count(*) as color
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
    dplyr::group_by(rsgl.log.bin.30.mpg) |>
    dplyr::summarize(
      rsgl.linear.avg.hp = mean(hp),
      rsgl.count = dplyr::n()
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned cols and aggs for multiple scales", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			bin(mpg) as y,
			count(*) as color,
			bin(mpg) as size
		from cars
		group by
			bin(mpg)
		using bars
		scale by
			log(y),
			ln(size)
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales
  int_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", input_df,
    scale = new_sgl_scale_log()
  )
  int_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", int_df,
    scale = new_sgl_scale_ln()
  )
  transformed_df <- add_transformed_column(
    new_sgl_cta_bin(), "mpg", int_df,
    scale = new_sgl_scale_linear()
  )


  result_df <- perform_as_for_layer(
    layer, transformed_df, scales
  )

  expected_df <- transformed_df |>
    dplyr::group_by(
      rsgl.log.bin.30.mpg,
      rsgl.ln.bin.30.mpg,
      rsgl.linear.bin.30.mpg
    ) |>
    dplyr::summarize(
      rsgl.count = dplyr::n()
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns binned and unbinned col and aggs", {
  rgs <- sgl_to_rgs("
		visualize
			bin(mpg) as x,
			count(*) as y,
			vs_cat as color,
			avg(hp) as size
		from (
			select
				*,
				cast(vs as varchar) as vs_cat
			from cars
		)
		group by
			bin(mpg),
			vs_cat
		using points
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
    dplyr::group_by(rsgl.linear.bin.30.mpg, vs_cat) |>
    dplyr::summarize(
      rsgl.count = dplyr::n(),
      rsgl.linear.avg.hp = mean(hp)
    )

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
    dplyr::group_by(rsgl.linear.bin.30.mpg, vs_cat) |>
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
    dplyr::group_by(vs_cat, rsgl.linear.bin.30.mpg) |>
    dplyr::summarize(rsgl.count = dplyr::n())

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns aggs from collection", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		group by
			hp,
			mpg
		collect by
			count(*),
			avg(cyl)
		using lines
	")
  dfs <- result_dfs(rgs, test_con)
  layer <- rgs$layers[[1]]
  input_df <- dfs[[1]]
  scales <- rgs$scales

  result_df <- perform_as_for_layer(
    layer, input_df, scales
  )

  expected_df <- input_df |>
    dplyr::group_by(hp, mpg) |>
    dplyr::summarize(
      rsgl.count = dplyr::n(),
      rsgl.linear.avg.cyl = mean(cyl)
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("returns aggs from mapping and collection without duplication", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y,
			avg(hp) as color
		from cars
		group by
			hp,
			mpg
		collect by
			count(*),
			avg(hp)
		using lines
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
    dplyr::group_by(hp, mpg) |>
    dplyr::summarize(
      rsgl.count = dplyr::n(),
      rsgl.linear.avg.hp = mean(hp)
    )

  expect_equal_ignore_order(result_df, expected_df)
})

test_that("takes scales into account for agg", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y,
			avg(cyl) as color
		from cars
		group by
			hp,
			mpg
		collect by
			avg(cyl),
			avg(vs)
		using lines

		scale by
			log(color)
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
    dplyr::mutate(rsgl.log.cyl = log10(cyl)) |>
    dplyr::group_by(hp, mpg) |>
    dplyr::summarize(
      rsgl.linear.avg.vs = mean(vs),
      rsgl.log.avg.cyl = mean(rsgl.log.cyl)
    ) |>
    dplyr::mutate(rsgl.log.avg.cyl = 10^rsgl.log.avg.cyl)

  expect_equal_ignore_order(result_df, expected_df)
})
