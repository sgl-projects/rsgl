describe("dir_priority_ranking", {
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
        dir_priority_ranking(layer, df, "x"),
        expected_priority
      )
    })
  })
})

describe("ggplot_direction", {
  describe("direction qualifier is present", {
    it("returns direction from ggplot_dir_from_qual", {
      cases <- tibble::tribble(
        ~geom, ~qual, ~expected_dir,
        "line", "horizontal", "x",
        "line", "vertical", "y",
        "bar", "horizontal", "y",
        "bar", "vertical", "x"
      )
      purrr::pwalk(cases, function(geom, qual, expected_dir) {
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
          ggplot_direction(layer, df),
          expected_dir
        )
      })
    })
  })
  describe("direction qualifier is not present", {
    describe("one positional aesthetic", {
      it("returns correct dir for each pos aes", {
        cases <- tibble::tribble(
          ~aes, ~expected_dir,
          "x", "y",
          "y", "x",
          "theta", "y",
          "r", "x"
        )
        purrr::pwalk(cases, function(aes, expected_dir) {
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
            ggplot_direction(layer, df),
            expected_dir
          )
        })
      })
    })
    describe("two positional aesthetics", {
      describe("box with collection on one pos aes", {
        it("returns correct dir based on collected aes", {
          cases <- tibble::tribble(
            ~collected_aes, ~uncollected_aes, ~expected_dir,
            "x", "y", "x",
            "y", "x", "y",
            "theta", "r", "x",
            "r", "theta", "y"
          )
          purrr::pwalk(
            cases,
            function(collected_aes, uncollected_aes, expected_dir) {
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
                ggplot_direction(layer, df),
                expected_dir
              )
            }
          )
        })
      })
      describe("not box with collection on one pos aes", {
        it("returns dir based on priority ranking", {
          # tie goes to x
          cases <- tibble::tribble(
            ~aes_1, ~aes_2, ~expr_1, ~expr_2, ~expected_dir,
            "x", "y", "day", "number", "x",
            "y", "x", "day", "number", "y",
            "x", "y", "day", "day_and_time", "x",
            "theta", "r", "day", "number", "x",
            "r", "theta", "day", "number", "y",
            "theta", "r", "day", "day_and_time", "x"
          )
          purrr::pwalk(
            cases,
            function(aes_1, aes_2, expr_1, expr_2, expected_dir) {
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
                ggplot_direction(layer, df),
                expected_dir
              )
            }
          )
        })
      })
    })
  })
})
