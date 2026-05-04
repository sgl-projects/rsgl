describe("perform_cts_for_layer", {
  describe("no transformations in any clause", {
    it("returns original dataframe", {
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
  })
  describe("transformations in visualize clause only", {
    describe("single transformation expression", {
      it("adds transformed column with default scaling", {
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
      it("adds transformed column with non-default scaling", {
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
      it("adds transformed column with arg", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg, 5) as x
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
          num_bins = 5,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    describe("multiple transformation expressions", {
      describe("all col expressions are the same", {
        describe("all scales are the same", {
          it("doesn't add duplicate transformed columns", {
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
        })
        describe("different scales are present", {
          it("adds a transformed column for each scale", {
            rgs <- sgl_to_rgs("
							visualize
								bin(mpg) as x,
								bin(mpg) as y
							from cars
							using points
							scale by
								log(y)
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
            expected_df <- add_transformed_column(
              new_sgl_cta_bin(),
              "mpg",
              expected_df,
              scale = new_sgl_scale_linear()
            )

            expect_equal(result_df, expected_df)
          })
        })
        describe("transformation col expressions are different", {
          it("adds a transformed column for each", {
            rgs <- sgl_to_rgs("
							visualize
								bin(mpg) as x,
								bin(mpg, 5) as y,
								bin(hp) as color
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
              "hp",
              input_df,
              scale = new_sgl_scale_linear()
            )
            expected_df <- add_transformed_column(
              new_sgl_cta_bin(),
              "mpg",
              expected_df,
              num_bins = 5,
              scale = new_sgl_scale_linear()
            )
            expected_df <- add_transformed_column(
              new_sgl_cta_bin(),
              "mpg",
              expected_df,
              scale = new_sgl_scale_linear()
            )

            expect_equal(result_df, expected_df)
          })
        })
      })
    })
    it("ignores untransformed columns", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					bin(hp) as y,
					cyl as color
				from cars
				using points
				scale by
					log(color)
			")
      dfs <- result_dfs(rgs, test_con)
      layer <- rgs$layers[[1]]
      input_df <- dfs[[1]]
      scales <- rgs$scales

      result_df <- perform_cts_for_layer(layer, input_df, scales)

      expected_df <- add_transformed_column(
        new_sgl_cta_bin(),
        "hp",
        input_df,
        scale = new_sgl_scale_linear()
      )
      expected_df <- add_transformed_column(
        new_sgl_cta_bin(),
        "mpg",
        expected_df,
        scale = new_sgl_scale_linear()
      )

      expect_equal(result_df, expected_df)
    })
  })
  describe("transformations in grouping clause only", {
    describe("single transformation expression", {
      it("adds transformed column with default scaling", {
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
      it("adds transformed column with arg", {
        rgs <- sgl_to_rgs("
					visualize
						count(*) as x
					from cars
					group by
						bin(mpg, 5)
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
          num_bins = 5,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    describe("multiple transformation expressions", {
      it("adds transformed columns with default scaling", {
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

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "hp",
          input_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "mpg",
          expected_df,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    it("ignores untransformed expression", {
      rgs <- sgl_to_rgs("
				visualize
					cyl as x,
					count(*) as y
				from cars
				group by
					cyl,
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
  })
  describe("transformations in collect by clause only", {
    describe("single transformation expression", {
      it("adds transformed column with default scaling", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x,
						mpg as y
					from cars
					collect by
						bin(wt)
					using lines
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "wt",
          input_df,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
      it("adds transformed column with arg", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x,
						mpg as y
					from cars
					collect by
						bin(wt, 5)
					using lines
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "wt",
          input_df,
          num_bins = 5,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    describe("multiple transformation expressions", {
      it("adds transformed columns with default scaling", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x,
						mpg as y
					from cars
					collect by
						bin(wt),
						bin(disp)
					using lines
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "disp",
          input_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "wt",
          expected_df,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    it("ignores untransformed expression", {
      rgs <- sgl_to_rgs("
				visualize
					hp as x,
					mpg as y
				from cars
				collect by
					bin(wt),
					bin(disp),
					cyl
				using lines
			")
      dfs <- result_dfs(rgs, test_con)
      layer <- rgs$layers[[1]]
      input_df <- dfs[[1]]
      scales <- rgs$scales

      result_df <- perform_cts_for_layer(layer, input_df, scales)

      expected_df <- add_transformed_column(
        new_sgl_cta_bin(),
        "disp",
        input_df,
        scale = new_sgl_scale_linear()
      )
      expected_df <- add_transformed_column(
        new_sgl_cta_bin(),
        "wt",
        expected_df,
        scale = new_sgl_scale_linear()
      )

      expect_equal(result_df, expected_df)
    })
  })
  describe("transformations in visualize and group by clauses only", {
    describe("visualize and group by clause have same trans exprs", {
      it("doesn't add additional transformed cols for group exprs", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						count(*) as y,
						bin(hp, 5) as color
					from cars
					group by
						bin(mpg),
						bin(hp, 5)
					using points
					scale by
						log(color)
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "hp",
          input_df,
          num_bins = 5,
          scale = new_sgl_scale_log()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "mpg",
          expected_df,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    describe("group by clause has additional trans exprs", {
      it("adds additional transformed cols with default scaling", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						count(*) as y
					from cars
					group by
						bin(mpg),
						bin(mpg, 5),
						bin(hp)
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
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "hp",
          expected_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "mpg",
          expected_df,
          num_bins = 5,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
  })
  describe("transformations in visualize and collect by clauses only", {
    describe("visualize and collect by clause have same trans exprs", {
      it("doesn't add additional transformed cols for collect exprs", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						hp as y
					from cars
					collect by
						bin(mpg)
					using boxes
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
    })
    describe("visualize and collect by clause have different trans exprs", {
      it("adds all transformed cols (default scaling for collect only exprs)", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						hp as y,
						bin(disp) as color
					from cars
					collect by
						bin(disp),
						bin(mpg, 5),
						bin(wt)
					using points
					scale by
						log(color)
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "disp",
          input_df,
          scale = new_sgl_scale_log()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "mpg",
          expected_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "wt",
          expected_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "mpg",
          expected_df,
          num_bins = 5,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
  })
  describe("transformations in group by and collect by clauses only", {
    describe("group by and collect by clause have same trans exprs", {
      it("adds transformed columns with default scaling and no duplication", {
        rgs <- sgl_to_rgs("
					visualize
						cut as x,
						count(*) as y
					from diamonds
					group by
						bin(carat, 5),
						bin(price)
					collect by
						bin(carat, 5),
						bin(price)
					using lines
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "price",
          input_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "carat",
          expected_df,
          num_bins = 5,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
    describe("collect by has subset of group by clauses trans exprs", {
      it("adds transformed columns with default scaling and no duplication", {
        rgs <- sgl_to_rgs("
					visualize
						cut as x,
						count(*) as y
					from diamonds
					group by
						bin(carat),
						bin(price)
					collect by
						bin(price)
					using lines
				")
        dfs <- result_dfs(rgs, test_con)
        layer <- rgs$layers[[1]]
        input_df <- dfs[[1]]
        scales <- rgs$scales

        result_df <- perform_cts_for_layer(layer, input_df, scales)

        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "price",
          input_df,
          scale = new_sgl_scale_linear()
        )
        expected_df <- add_transformed_column(
          new_sgl_cta_bin(),
          "carat",
          expected_df,
          scale = new_sgl_scale_linear()
        )

        expect_equal(result_df, expected_df)
      })
    })
  })
  describe("transformations in visualize, group by, and collect by clauses", {
    it("adds all transformed columns without duplication", {
      rgs <- sgl_to_rgs("
				visualize
					bin(carat) as x,
					count(*) as y
				from diamonds
				group by
					bin(carat),
					bin(price)
				collect by
					bin(price)
				using lines
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
        "carat",
        input_df,
        scale = new_sgl_scale_log()
      )
      expected_df <- add_transformed_column(
        new_sgl_cta_bin(),
        "price",
        expected_df,
        scale = new_sgl_scale_linear()
      )

      expect_equal(result_df, expected_df)
    })
  })
})
