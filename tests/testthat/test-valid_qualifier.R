describe("valid_qualifier", {
  it("considers default valid", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points
		")
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer)
    )
  })
  it("considers valid qualifier valid", {
    rgs <- sgl_to_rgs("
				visualize
					hp as x,
					mpg as y
				from cars
				using jittered points
			")
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer)
    )
  })
  it("considers invalid qualifier invalid", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using regression boxes
		")
    layer <- rgs$layers[[1]]

    expect_error(
      valid_qualifier(layer),
      "Error: the regression qualifier is not valid for the box geom.",
      fixed = TRUE
    )
  })
})
