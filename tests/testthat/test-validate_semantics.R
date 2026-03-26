test_that("doesn't raise error for valid semantics", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    validate_semantics(rgs, dfs)
  )
})

test_that("raises error if column name doesn't exist", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      not_a_col as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    validate_semantics(rgs, dfs),
    "Error: referenced column 'not_a_col' not found",
    fixed = TRUE
  )
})

test_that("raises error for invalid cta", {
  rgs <- sgl_to_rgs("
    visualize
      bin(letter) as x,
      count(*) as y
    from synth
		group by
			bin(letter)
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: cannot apply bin to a categorical",
    "column, found bin(letter)."
  )
  expect_error(
    validate_semantics(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

test_that("raises error for invalid aesthetics", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as theta
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: found aesthetics from multiple coordinate systems.",
    "All positional aesthetics must be from a single coordinate system."
  )
  expect_error(
    validate_semantics(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

test_that("raises error for invalid qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using jittered bars
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    validate_semantics(rgs, dfs),
    "Error: the jittered qualifier is not valid for the bar geom.",
    fixed = TRUE
  )
})

test_that("raises error for invalid groupings", {
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

  expected_msg <- paste(
    "Error: if group by clause is provided then aggregation(s)",
    "must be present in the visualize clause."
  )
  expect_error(
    validate_semantics(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

test_that("raises error for invalid collections", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y,
			clarity as color
    from diamonds
    collect by
      cut
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: For the box geom, color mappings must have corresponding",
    "collections if an explicit collection is provided."
  )
  expect_error(
    validate_semantics(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

test_that("doesn't raise error for multiple layers with valid semantics", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points

		layer

		visualize
			hp as x,
			mpg as y
		from cars
		using regression line
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    validate_semantics(rgs, dfs)
  )
})

test_that(
  "raises error given multiple layers where a layer has invalid semantics",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points

			layer

			visualize
				number as x,
				letter as y
			from synth
			using points
		")
    dfs <- result_dfs(rgs, test_con)

    expected_msg <- paste(
      "Error: an aesthetic must be mapped to the same type",
      "(numerical, categorical, or temporal) across layers.",
      "Found the following types for the y aesthetic: numerical, categorical."
    )
    expect_error(
      validate_semantics(rgs, dfs),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that("raises error for invalid scaling", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using points
		scale by
			log(x)
  ")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: the log scale can only be applied",
    "to aesthetics with numerical mappings."
  )
  expect_error(
    validate_semantics(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

test_that("raises error for invalid faceting", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
		facet by
			cyl,
			vs,
			am
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    validate_semantics(rgs, dfs),
    "Error: cannot have more than two facets.",
    fixed = TRUE
  )
})

test_that("raises error for invalid title", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
		title
			color as 'Cylinders'
  ")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: title provided for aesthetic not found",
    "in any layer's aesthetic mapping: color."
  )
  expect_error(
    validate_semantics(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})
