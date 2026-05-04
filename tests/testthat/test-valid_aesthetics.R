describe("valid_aesthetics", {
  describe("no positional aes", {
    it("raises error", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as color
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_error(
        valid_aesthetics(layer),
        "Error: positional mapping(s) must be provided, none were found.",
        fixed = TRUE
      )
    })
  })
  describe("single positional aes", {
    it("considers x valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
    it("considers y valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as y
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
    it("considers theta valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as theta
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
    it("considers r valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as r
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
  })
  describe("two positional aes", {
    it("considers caretsian coordinates valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x,
					hp as y
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
    it("considers polar coordinates valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as theta,
					hp as r
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
    it("raises error for mixed coordinate systems", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x,
					hp as r
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expected_msg <- paste(
        "Error: found aesthetics from multiple coordinate systems.",
        "All positional aesthetics must be from a single coordinate system."
      )
      expect_error(
        valid_aesthetics(layer),
        expected_msg,
        fixed = TRUE
      )
    })
  })
  describe("more than two positional aes", {
    it("raises error", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x,
					hp as y,
					cyl as theta
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expected_msg <- paste(
        "Error: found aesthetics from multiple coordinate systems.",
        "All positional aesthetics must be from a single coordinate system."
      )
      expect_error(
        valid_aesthetics(layer),
        expected_msg,
        fixed = TRUE
      )
    })
  })
  describe("non-positional aes", {
    it("considers valid non-pos aes valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x,
					hp as y,
					cyl as color
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_aesthetics(layer)
      )
    })
    it("considers invalid non-pos aes invalid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x,
					hp as y,
					cyl as size
				from cars
				using lines
			")
      layer <- rgs$layers[[1]]

      expect_error(
        valid_aesthetics(layer),
        "Error: the size aesthetic is not valid for the line geom.",
        fixed = TRUE
      )
    })
  })
})
