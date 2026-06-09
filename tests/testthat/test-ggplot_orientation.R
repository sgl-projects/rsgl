describe("orntn_priority_ranking", {
  it("gives correct priority for each category", {
    cases <- tibble::tribble(
      ~col_name, ~expected_priority,
      "day", 1,
      "letter", 2,
      "bin(number)", 3,
      "number", 4
    )
    purrr::pwalk(cases, function(col_name, expected_priority) {
      sgl <- sprintf(
        "
					visualize
						%s as x
					from synth
					using bars
				",
        col_name
      )
      rgs <- sgl_to_rgs(sgl)
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expect_equal(
        orntn_priority_ranking(layer, df, "x"),
        expected_priority
      )
    })
  })
})

describe("ggplot_orientation", {
  describe("direction qualifier is present", {
    it("returns orientation from ggplot_orntn_from_qual", {
      cases <- tibble::tribble(
        ~geom, ~qual, ~expected_orientation,
        "line", "horizontal", "x",
        "line", "vertical", "y",
        "bar", "horizontal", "y",
        "bar", "vertical", "x"
      )
      purrr::pwalk(cases, function(geom, qual, expected_orientation) {
        sgl <- sprintf(
          "
						visualize
							hp as x,
							mpg as y
						from cars
						using %s %s
					",
          qual,
          geom
        )
        rgs <- sgl_to_rgs(sgl)
        layer <- rgs$layers[[1]]
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]

        expect_equal(
          ggplot_orientation(layer, df),
          expected_orientation
        )
      })
    })
  })
  describe("direction qualifier is not present", {
    describe("one positional aesthetic", {
      it("returns correct orientation for each pos aes", {
        cases <- tibble::tribble(
          ~aes, ~expected_orientation,
          "x", "y",
          "y", "x",
          "theta", "y",
          "r", "x"
        )
        purrr::pwalk(cases, function(aes, expected_orientation) {
          sgl <- sprintf(
            "
							visualize
								hp as %s
							from cars
							using bars
						",
            aes
          )
          rgs <- sgl_to_rgs(sgl)
          layer <- rgs$layers[[1]]
          dfs <- result_dfs(rgs, test_con)
          df <- dfs[[1]]

          expect_equal(
            ggplot_orientation(layer, df),
            expected_orientation
          )
        })
      })
    })
    describe("two positional aesthetics", {
      describe("box with collection on one pos aes", {
        it("returns correct orientation based on collected aes", {
          cases <- tibble::tribble(
            ~collected_aes, ~uncollected_aes, ~expected_orientation,
            "x", "y", "x",
            "y", "x", "y",
            "theta", "r", "x",
            "r", "theta", "y"
          )
          purrr::pwalk(
            cases,
            function(collected_aes, uncollected_aes, expected_orientation) {
              sgl <- sprintf(
                "
									visualize
										cyl as %s,
										mpg as %s
									from cars
									collect by
										cyl
									using boxes
								",
                collected_aes,
                uncollected_aes
              )
              rgs <- sgl_to_rgs(sgl)
              layer <- rgs$layers[[1]]
              dfs <- result_dfs(rgs, test_con)
              df <- dfs[[1]]

              expect_equal(
                ggplot_orientation(layer, df),
                expected_orientation
              )
            }
          )
        })
      })
      describe("not box with collection on one pos aes", {
        it("returns orientation based on priority ranking", {
          # tie goes to x
          cases <- tibble::tribble(
            ~aes_1, ~aes_2, ~expr_1, ~expr_2, ~expected_orientation,
            "x", "y", "day", "number", "x",
            "y", "x", "day", "number", "y",
            "x", "y", "day", "day_and_time", "x",
            "theta", "r", "day", "number", "x",
            "r", "theta", "day", "number", "y",
            "theta", "r", "day", "day_and_time", "x"
          )
          purrr::pwalk(
            cases,
            function(aes_1, aes_2, expr_1, expr_2, expected_orientation) {
              sgl <- sprintf(
                "
									visualize
										%s as %s,
										%s as %s
									from synth
									using boxes
								",
                expr_1,
                aes_1,
                expr_2,
                aes_2
              )
              rgs <- sgl_to_rgs(sgl)
              layer <- rgs$layers[[1]]
              dfs <- result_dfs(rgs, test_con)
              df <- dfs[[1]]

              expect_equal(
                ggplot_orientation(layer, df),
                expected_orientation
              )
            }
          )
        })
      })
    })
  })
})
