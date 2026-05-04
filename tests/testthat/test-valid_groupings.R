describe("valid_groupings", {
  describe("group by clause is present", {
    describe("viz and collect unagg exprs are proper subsets of group exprs", {
      it("doesn't raise error", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x
					from cars
					group by
						bin(mpg),
						hp
					collect by
						bin(mpg)
					using lines
				")
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_groupings(layer)
        )
      })
    })
    describe("viz and collect unagg exprs are same as group exprs", {
      it("doesn't raise error", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						hp as y
					from cars
					group by
						bin(mpg),
						hp
					collect by
						bin(mpg),
						hp
					using lines
				")
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_groupings(layer)
        )
      })
    })
    describe("viz unagg exprs are not subset of group exprs", {
      it("raises error", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						hp as y,
						cyl as color
					from cars
					group by
						bin(mpg),
						hp
					collect by
						bin(mpg),
						hp
					using lines
				")
        layer <- rgs$layers[[1]]

        expected_msg <- paste(
          "Error: unaggregated expressions in the visualize",
          "and collect by clauses must also be present in the",
          "group by clause."
        )
        expect_error(
          valid_groupings(layer),
          expected_msg,
          fixed = TRUE
        )
      })
    })
    describe("collect unagg exprs are not subset of group exprs", {
      it("raises error", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						hp as y
					from cars
					group by
						bin(mpg),
						hp
					collect by
						bin(mpg),
						hp,
						cyl
					using lines
				")
        layer <- rgs$layers[[1]]

        expected_msg <- paste(
          "Error: unaggregated expressions in the visualize",
          "and collect by clauses must also be present in the",
          "group by clause."
        )
        expect_error(
          valid_groupings(layer),
          expected_msg,
          fixed = TRUE
        )
      })
    })
    it("it ignores aggregated expressions in viz and collect clauses", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					hp as y,
					count(*) as color
				from cars
				group by
					bin(mpg),
					hp
				collect by
					bin(mpg),
					hp,
					count(*)
				using lines
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_groupings(layer)
      )
    })
    it("raises error for aggregation in group by clause", {
      rgs <- sgl_to_rgs("
				visualize
					bin(mpg) as x,
					hp as y,
					avg(cyl) as color
				from cars
				group by
					bin(mpg),
					hp,
					avg(cyl)
				collect by
					bin(mpg),
					hp,
					avg(cyl)
				using lines
			")
      layer <- rgs$layers[[1]]

      expect_error(
        valid_groupings(layer),
        "Error: group by clause cannot contain aggregation expressions.",
        fixed = TRUE
      )
    })
  })
  describe("group by clause is omitted", {
    describe("no aggregations are present", {
      it("doesn't raise an error", {
        rgs <- sgl_to_rgs("
					visualize
						bin(mpg) as x,
						hp as y
					from cars
					collect by
						bin(mpg),
						hp
					using lines
				")
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_groupings(layer)
        )
      })
    })
    describe("aggregations are present", {
      describe("all viz and collect exprs are aggregations", {
        it("doesn't raise an error", {
          rgs <- sgl_to_rgs("
						visualize
							count(*) as x,
							avg(mpg) as y
						from cars
						collect by
							count(*)
						using lines
					")
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_groupings(layer)
          )
        })
      })
      describe("not all viz exprs are aggregations", {
        it("raises an error", {
          rgs <- sgl_to_rgs("
						visualize
							bin(mpg) as x,
							count(*) as y
						from cars
						collect by
							count(*)
						using lines
					")
          layer <- rgs$layers[[1]]

          expected_msg <- paste(
            "Error: given that aggregations are present,",
            "all unaggregated expressions in the visualize",
            "and collect by clause must also be included",
            "in the group by clause.",
            "However, no group by clause was provided."
          )
          expect_error(
            valid_groupings(layer),
            expected_msg,
            fixed = TRUE
          )
        })
      })
      describe("not all collect exprs are aggregations", {
        it("raises an error", {
          rgs <- sgl_to_rgs("
						visualize
							count(*) as x,
							count(*) as y
						from cars
						collect by
							bin(mpg),
							count(*)
						using lines
					")
          layer <- rgs$layers[[1]]

          expected_msg <- paste(
            "Error: given that aggregations are present,",
            "all unaggregated expressions in the visualize",
            "and collect by clause must also be included",
            "in the group by clause.",
            "However, no group by clause was provided."
          )
          expect_error(
            valid_groupings(layer),
            expected_msg,
            fixed = TRUE
          )
        })
      })
    })
  })
})
