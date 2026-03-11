test_that("raises error for aggregation in grouping expression", {
  rgs <- sgl_to_rgs("
    visualize
			bin(mpg) as x,
			count(*) as y
    from cars
		group by
			bin(mpg),
			count(*)
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_groupings(layer, df),
    "Error: expressions in the group by clause cannot be aggregations.",
    fixed = TRUE
  )
})

describe("valid_groupings", {
  describe("no aggregations with no groupings", {
    it("does not raise an error", {
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

      expect_no_error(
        valid_groupings(layer, df)
      )
    })
  })

  describe("aggregations with no groupings", {
    it("does not raise an error if all mappings are aggregations", {
      rgs <- sgl_to_rgs("
				visualize
					count(*) as x,
					count(*) as y,
					count(*) as color
				from cars
				using points
			")
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_groupings(layer, df)
      )
    })
    describe("not all mappings are aggregations", {
      it("raises error for mapping with no cta", {
        rgs <- sgl_to_rgs("
					visualize
						cut as x,
						count(*) as y
					from diamonds
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        layer <- rgs$layers[[1]]

        expected_msg <- paste(
          "Error: if aggregations are present, then all unaggregated",
          "expressions in the visualize clause must be included",
          "in the group by clause."
        )
        expect_error(
          valid_groupings(layer, df),
          expected_msg,
          fixed = TRUE
        )
      })
      it("raises error for mapping with column-level transformation", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						count(*) as y
					from cars
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        layer <- rgs$layers[[1]]

        expected_msg <- paste(
          "Error: if aggregations are present, then all unaggregated",
          "expressions in the visualize clause must be included",
          "in the group by clause."
        )
        expect_error(
          valid_groupings(layer, df),
          expected_msg,
          fixed = TRUE
        )
      })
    })
  })

  describe("no aggregations with groupings", {
    it("raises error", {
      rgs <- sgl_to_rgs("
				visualize
					hp as x,
					mpg as y
				from cars
				group by
					hp
				using points
			")
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]
      layer <- rgs$layers[[1]]

      expected_msg <- paste(
        "Error: if group by clause is provided then aggregation(s)",
        "must be present in the visualize clause."
      )
      expect_error(
        valid_groupings(layer, df),
        expected_msg,
        fixed = TRUE
      )
    })
  })

  describe("aggregations with groupings", {
    it(
      paste(
        "doesn't raise error if grouping expressions are",
        "equivalent to non-aggregated visualize expressions"
      ),
      {
        rgs <- sgl_to_rgs("
					visualize
						bin(price) as x,
						count(*) as y,
						cut as color
					from cars
					group by
						bin(price),
						cut
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_groupings(layer, df)
        )
      }
    )
    it(
      paste(
        "raises error if non-aggregated expression from",
        "visualize clause is not found in group by clause"
      ),
      {
        rgs <- sgl_to_rgs("
					visualize
						bin(price) as x,
						count(*) as y,
						cut as color
					from cars
					group by
						bin(price)
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        layer <- rgs$layers[[1]]

        expected_msg <- paste(
          "Error: if aggregations are present, then all unaggregated",
          "expressions in the visualize clause must be included",
          "in the group by clause."
        )
        expect_error(
          valid_groupings(layer, df),
          expected_msg,
          fixed = TRUE
        )
      }
    )
    it(
      paste(
        "doesn't raise error for additional grouping expressions",
        "that aren't in the visualize clause"
      ),
      {
        rgs <- sgl_to_rgs("
					visualize
						count(*) as x,
						count(*) as color
					from cars
					group by
						bin(price),
						cut
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_groupings(layer, df)
        )
      }
    )
  })
})
