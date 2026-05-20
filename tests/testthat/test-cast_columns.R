describe("aes_has_dt_and_ts", {
  describe("single layer", {
    it("returns FALSE", {
      rgs <- sgl_to_rgs("
				visualize
					day as x
				from synth
				using points
			")
      dfs <- result_dfs(rgs, test_con)

      expect_false(
        aes_has_dt_and_ts("x", rgs$layers, dfs)
      )
    })
  })
  describe("multiple layers", {
    describe("aes is only in one layer", {
      it("returns FALSE", {
        rgs <- sgl_to_rgs("
					visualize
						letter as x,
						number as y,
						day as color
					from synth
					using points

					layer

					visualize
						letter as x,
						number as y
					from synth
					using line
				")
        dfs <- result_dfs(rgs, test_con)

        expect_false(
          aes_has_dt_and_ts("color", rgs$layers, dfs)
        )
      })
    })
    describe("aes is in multiple layers", {
      describe("mappings are not temporal", {
        it("returns FALSE", {
          rgs <- sgl_to_rgs("
						visualize
							letter as x
						from synth
						using points

						layer

						visualize
							boolean as x
						from synth
						using points
					")
          dfs <- result_dfs(rgs, test_con)

          expect_false(
            aes_has_dt_and_ts("x", rgs$layers, dfs)
          )
        })
      })
      describe("mappings are temporal", {
        describe("mappings are to dates", {
          it("returns FALSE", {
            rgs <- sgl_to_rgs("
							visualize
								day as x
							from synth
							using points

							layer

							visualize
								date as x
							from economics
							using points
						")
            dfs <- result_dfs(rgs, test_con)

            expect_false(
              aes_has_dt_and_ts("x", rgs$layers, dfs)
            )
          })
        })
        describe("mappings are to timestamps", {
          it("returns FALSE", {
            rgs <- sgl_to_rgs("
							visualize
								day_and_time as x
							from synth
							using points

							layer

							visualize
								day_and_time as x
							from (
								select
									cast(\"date\" as timestamp) as day_and_time
								from economics
							)
							using points
						")
            dfs <- result_dfs(rgs, test_con)

            expect_false(
              aes_has_dt_and_ts("x", rgs$layers, dfs)
            )
          })
        })
        describe("mappings are to both date and timestamp", {
          it("returns TRUE", {
            rgs <- sgl_to_rgs("
							visualize
								day_and_time as x
							from synth
							using points

							layer

							visualize
								day as x
							from synth
							using points
						")
            dfs <- result_dfs(rgs, test_con)

            expect_true(
              aes_has_dt_and_ts("x", rgs$layers, dfs)
            )
          })
        })
      })
    })
  })
})

describe("aes_with_dt_and_ts", {
  describe("no aes has date and timestamp", {
    it("returns empty character vector", {
      rgs <- sgl_to_rgs("
				visualize
					letter as x,
					number as y
				from synth
				using points
			")
      dfs <- result_dfs(rgs, test_con)

      expect_equal(
        aes_with_dt_and_ts(rgs, dfs),
        character(0)
      )
    })
  })
  describe("at least one aes has date and timestamp", {
    it("returns aes's with date and timestamp", {
      rgs <- sgl_to_rgs("
				visualize
					letter as x,
					number as y,
					day as color,
					day_and_time as size
				from synth
				using points

				layer

				visualize
					letter as x,
					number as y,
					day_and_time as color,
					day as size
				from synth
				using points

			")
      dfs <- result_dfs(rgs, test_con)

      expect_setequal(
        aes_with_dt_and_ts(rgs, dfs),
        c("color", "size")
      )
    })
  })
})

describe("cast_for_aes", {
  it("returns correct number of casted dfs", {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				number as y
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day as color
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day_and_time as color
			from synth
			using points

		")
    dfs <- result_dfs(rgs, test_con)

    actual_dfs <- cast_for_aes("color", rgs$layers, dfs)

    expect_equal(
      length(actual_dfs),
      3
    )
  })
  it("returns df of layer without aes unchanged", {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				number as y
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day as color
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day_and_time as color
			from synth
			using points

		")
    dfs <- result_dfs(rgs, test_con)

    actual_df <- cast_for_aes("color", rgs$layers, dfs)[[1]]

    expected_df <- dfs[[1]]
    expect_equal(
      actual_df,
      expected_df
    )
  })
  it("returns casted df of layer with date for aes", {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				number as y
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day as color
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day_and_time as color
			from synth
			using points

		")
    dfs <- result_dfs(rgs, test_con)

    actual_df <- cast_for_aes("color", rgs$layers, dfs)[[2]]

    expected_df <- dfs[[2]] |>
      dplyr::mutate(day = lubridate::as_datetime((day)))
    expect_equal(
      actual_df,
      expected_df
    )
  })
  it("returns df of layer with timestamp for aes unchanged", {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				number as y
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day as color
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day_and_time as color
			from synth
			using points

		")
    dfs <- result_dfs(rgs, test_con)

    actual_df <- cast_for_aes("color", rgs$layers, dfs)[[3]]

    expected_df <- dfs[[3]]
    expect_equal(
      actual_df,
      expected_df
    )
  })
})

describe("cast_columns", {
  it("casts dates for aes's with both date and timestamp", {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				number as y
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day as color,
				day_and_time as size
			from synth
			using points

			layer

			visualize
				letter as x,
				number as y,
				day_and_time as color,
				day as size
			from synth
			using points

		")
    dfs <- result_dfs(rgs, test_con)

    actual_dfs <- cast_columns(rgs, dfs)

    expected_df_1 <- dfs[[1]]
    expected_df_2 <- dfs[[2]] |>
      dplyr::mutate(day = lubridate::as_datetime(day))
    expected_df_3 <- dfs[[3]] |>
      dplyr::mutate(day = lubridate::as_datetime(day))

    expect_equal(length(actual_dfs), 3)
    expect_equal(actual_dfs[[1]], expected_df_1)
    expect_equal(actual_dfs[[2]], expected_df_2)
    expect_equal(actual_dfs[[3]], expected_df_3)
  })
})
