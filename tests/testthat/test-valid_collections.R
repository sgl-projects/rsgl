describe("valid_collections", {
  describe("collect by clause is omitted", {
    describe("geom is collective", {
      it("doesn't raise error", {
        rgs <- sgl_to_rgs("
          visualize
            hp as x,
						mpg as y
          from cars
          using line
        ")
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_collections(layer)
        )
      })
    })
    describe("geom is not collective", {
      it("doesn't raise error", {
        rgs <- sgl_to_rgs("
          visualize
            hp as x,
						mpg as y
          from cars
          using points
        ")
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_collections(layer)
        )
      })
    })
  })
  describe("collect by clause is present", {
    describe("geom is not collective", {
      it("raises error", {
        rgs <- sgl_to_rgs("
          visualize
            hp as x,
						mpg as y
          from cars
					collect by
						cyl
          using points
        ")
        layer <- rgs$layers[[1]]

        expected_msg <- paste(
          "Error: collect by clause should not be provided",
          "for non-collective geom point."
        )
        expect_error(
          valid_collections(layer),
          expected_msg,
          fixed = TRUE
        )
      })
    })
    describe("geom is collective", {
      describe("non-positional aes exprs are proper subset of collections", {
        it("doesn't raise error", {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y
						from cars
						collect by
							cyl
						using lines
					")
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_collections(layer)
          )
        })
      })
      describe("non-positional aes exprs are same as collections", {
        it("doesn't raise error", {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y,
							cyl as color
						from cars
						collect by
							cyl
						using lines
					")
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_collections(layer)
          )
        })
      })
      describe("non-positional aes exprs are not a subset of collections", {
        it("raises error", {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y,
							cyl as color,
							am as size
						from cars
						collect by
							cyl
						using lines
					")
          layer <- rgs$layers[[1]]

          expected_msg <- paste(
            "Error: all expressions mapped to from non-positional aesthetics",
            "must be included in the collect by clause."
          )
          expect_error(
            valid_collections(layer),
            expected_msg,
            fixed = TRUE
          )
        })
      })
      describe("uncollected pos aes count is less than geom extension", {
        it("doesn't raise error", {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y
						from cars
						collect by
							hp,
							cyl
						using lines
					")
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_collections(layer)
          )
        })
      })
      describe("uncollected pos aes count is equal to geom extension", {
        it("doesn't raise error", {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y
						from cars
						collect by
							cyl
						using lines
					")
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_collections(layer)
          )
        })
      })
      describe("uncollected pos aes count exceeds geom extension", {
        it("raises error", {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y
						from cars
						collect by
							cyl
						using boxes
					")
          layer <- rgs$layers[[1]]

          expected_msg <- paste(
            "Error: the number of uncollected positional aesthetic",
            "expressions exceeds the extensionality of the box geom.",
            "Add an expression mapped to from a positional aesthetic",
            "to the collect by clause."
          )
          expect_error(
            valid_collections(layer),
            expected_msg,
            fixed = TRUE
          )
        })
      })
    })
  })
})
