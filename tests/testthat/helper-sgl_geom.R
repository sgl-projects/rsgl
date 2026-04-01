# nolint start: object_usage_linter.
valid_qualifier_tests <- function(geom, valid_qualifiers) {
  describe("valid_qualifier", {
    it("considers default valid", {
      rgs <- sgl_to_rgs("
				visualize
					mpg as x,
					hp as y
				from cars
				using points
			")
      layer <- rgs$layers[[1]]

      expect_no_error(
        valid_qualifier(geom, layer)
      )
    })
    it("considers valid qualifiers valid", {
      for (qualifier in valid_qualifiers) {
        sgl <- sprintf(
          "
						visualize
							mpg as x,
							hp as y
						from cars
						using %s points
					",
          qualifier
        )
        rgs <- sgl_to_rgs(sgl)
        layer <- rgs$layers[[1]]

        expect_no_error(
          valid_qualifier(geom, layer)
        )
      }
    })
    it("considers invalid qualifiers invalid", {
      invalid_qualifiers <- setdiff(.all_quals, valid_qualifiers)
      for (qualifier in invalid_qualifiers) {
        sgl <- sprintf(
          "
						visualize
							mpg as x,
							hp as y
						from cars
						using %s points
					",
          qualifier
        )
        rgs <- sgl_to_rgs(sgl)
        layer <- rgs$layers[[1]]

        expected_msg <- sprintf(
          "Error: the %s qualifier is not valid for the %s geom.",
          qualifier,
          geom_name(geom)
        )
        expect_error(
          valid_qualifier(geom, layer),
          expected_msg,
          fixed = TRUE
        )
      }
    })
  })
}
# nolint end
