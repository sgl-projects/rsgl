test_that("valid_scales doesn't raise error for default scales", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y,
			cyl as color
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
    valid_scales(rgs, dfs)
  )
})

test_that("valid_scales doesn't raise error for valid explicit scales", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using points

		layer

		visualize
			hp as x,
			mpg as y
		from cars
		using regression line

		scale by
			linear(x),
			log(color)
	")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_scales(rgs, dfs)
  )
})

test_that("valid_scales raises error for invalid explicit scale", {
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

		scale by
			linear(x),
			log(color)
	")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_scales(rgs, dfs),
    "Error: a scaled aesthetic must have at least one mapping",
    fixed = TRUE
  )
})
