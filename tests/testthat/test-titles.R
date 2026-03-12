default_title_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr, ~expected_title,
    "no cta", "mpg", "mpg",
    "bin", "bin(mpg)", "Binned mpg",
    "count", "count(*)", "Count"
  )
}
patrick::with_parameters_test_that(
  "default_title returns correct title:",
  {
    sgl <- sprintf(
      "
        visualize
          %s as x
        from cars
        using points
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    col_expr <- rgs$layers[[1]]$aes_mappings$x

    expect_equal(
      default_title(col_expr),
      expected_title
    )
  },
  .cases = default_title_cases()
)

test_that("title_for_aes returns explicit title", {
  rgs <- sgl_to_rgs("
		visualize
			mpg as x
		from cars
		using points

		title
			x as 'Miles Per Gallon'
	")

  expect_equal(
    title_for_aes("x", rgs),
    "Miles Per Gallon"
  )
})

patrick::with_parameters_test_that(
  "title_for_aes returns default title if no explicit title is provided:",
  {
    sgl <- sprintf(
      "
        visualize
          %s as x
        from cars
        using points
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)

    expect_equal(
      title_for_aes("x", rgs),
      expected_title
    )
  },
  .cases = default_title_cases()
)

test_that("title_for_aes skips layer without aes", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using regression line

		layer

		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using points
	")

  expect_equal(
    title_for_aes("color", rgs),
    "cyl"
  )
})

test_that("title_for_aes uses title from first layer if present in multiple", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using points

		layer

		visualize
			bin(hp) as x,
			count(*) as y
		from cars
		using bars
	")

  expect_equal(
    title_for_aes("x", rgs),
    "hp"
  )
})

patrick::with_parameters_test_that(
  "title_for_aes returns title for any aesthetic:",
  {
    other_aes <- ifelse(aes == "x", "y", "x")
    sgl <- sprintf(
      "
        visualize
					hp as %s,
          mpg as %s
        from cars
        using points
			",
      other_aes,
      aes
    )
    rgs <- sgl_to_rgs(sgl)

    expect_equal(
      title_for_aes(aes, rgs),
      "mpg"
    )
  },
  aes = .all_aes,
  .test_name = aes
)

test_that("ggplot_labs returns object of labels class", {
  rgs <- sgl_to_rgs("
		visualize
			mpg as x
		from cars
		using points
	")

  expect_equal(
    class(ggplot_labs(rgs))[1],
    "ggplot2::labels"
  )
})

test_that("ggplot_labs returns lab for explicit title", {
  rgs <- sgl_to_rgs("
		visualize
			mpg as x
		from cars
		using points

		title
			x as 'Miles Per Gallon'
	")

  actual_labs <- ggplot_labs(rgs)

  expect_equal(
    actual_labs$x,
    "Miles Per Gallon"
  )
})

patrick::with_parameters_test_that(
  "ggplot_labs returns lab with default title if no explicit title provided:",
  {
    sgl <- sprintf(
      "
        visualize
          %s as x
        from cars
        using points
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs$x,
      expected_title
    )
  },
  .cases = default_title_cases()
)

test_that("ggplot_labs skips layer without aes", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using regression line

		layer

		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using points
	")

  actual_labs <- ggplot_labs(rgs)

  expect_equal(
    actual_labs$colour,
    "cyl"
  )
})

test_that("ggplot_labs uses title from first layer if present in multiple", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using points

		layer

		visualize
			bin(hp) as x,
			count(*) as y
		from cars
		using bars
	")

  actual_labs <- ggplot_labs(rgs)

  expect_equal(
    actual_labs$x,
    "hp"
  )
})

patrick::with_parameters_test_that(
  "ggplot_labs returns lab for aesthetics where lab key matches aes name:",
  {
    other_aes <- ifelse(aes == "x", "y", "x")
    sgl <- sprintf(
      "
        visualize
					hp as %s,
          mpg as %s
        from cars
        using points
			",
      other_aes,
      aes
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs[[aes]],
      "mpg"
    )
  },
  aes = c("x", "y", "size"),
  .test_name = aes
)

polar_cases <- function() {
  tibble::tribble(
    ~.test_name, ~aes, ~expected_key,
    "theta", "theta", "x",
    "r", "r", "y"
  )
}
patrick::with_parameters_test_that(
  "ggplot_labs returns correct lab key for polar positional aes:",
  {
    sgl <- sprintf(
      "
				visualize
					mpg as %s
				from cars
				using points
			",
      aes
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs[[expected_key]],
      "mpg"
    )
    expect_false(aes %in% names(actual_labs))
  },
  .cases = polar_cases()
)

color_cases <- function() {
  tibble::tribble(
    ~.test_name, ~geom_expr, ~other_geom_expr, ~expected_key, ~unexpected_key,
    "bar geom", "bar", "point", "fill", "colour",
    "non-bar geom", "point", "bar", "colour", "fill"
  )
}
patrick::with_parameters_test_that(
  "ggplot_labs returns correct lab for explicit color title:",
  {
    sgl <- sprintf(
      "
				visualize
					mpg as color
				from cars
				using %s

				title
					color as 'Miles Per Gallon'
			",
      geom_expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs[[expected_key]],
      "Miles Per Gallon"
    )
    expect_false(unexpected_key %in% names(actual_labs))
  },
  .cases = color_cases()
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_labs returns fill lab with default title if",
    "no explicit color title is provided for bar geom:"
  ),
  {
    sgl <- sprintf(
      "
        visualize
          %s as color
        from cars
        using bars
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs$fill,
      expected_title
    )
    expect_false("colour" %in% names(actual_labs))
  },
  .cases = default_title_cases()
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_labs returns colour lab with default title",
    "if no explicit color title is provided for non-bar geom:"
  ),
  {
    sgl <- sprintf(
      "
        visualize
          %s as color
        from cars
        using points
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs$colour,
      expected_title
    )
    expect_false("fill" %in% names(actual_labs))
  },
  .cases = default_title_cases()
)

patrick::with_parameters_test_that("ggplot_labs skips layer without color aes:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as x,
					mpg as y
				from cars
				using %s

				layer

				visualize
					hp as x,
					mpg as y,
					cyl as color
				from cars
				using %s
			",
      geom_expr,
      geom_expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs[[expected_key]],
      "cyl"
    )
    expect_false(unexpected_key %in% names(actual_labs))
  },
  .cases = color_cases()
)

patrick::with_parameters_test_that(
  "ggplot_labs uses color title from first layer if present in multiple:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as color
				from cars
				using %s

				layer

				visualize
					bin(hp) as color
				from cars
				using %s
			",
      geom_expr,
      geom_expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs[[expected_key]],
      "hp"
    )
    expect_false(unexpected_key %in% names(actual_labs))
  },
  .cases = color_cases()
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_labs chooses lab key correctly when both geom",
    "types present but color mapping is only to:"
  ),
  {
    sgl <- sprintf(
      "
				visualize
					hp as x,
					mpg as y,
					cyl as color
				from cars
				using %s

				layer

				visualize
					hp as x,
					mpg as y
				from cars
				using %s
			",
      geom_expr,
      other_geom_expr
    )
    rgs <- sgl_to_rgs(sgl)

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs[[expected_key]],
      "cyl"
    )
    expect_false(unexpected_key %in% names(actual_labs))
  },
  .cases = color_cases()
)

test_that(
  paste(
    "ggplot_labs returns fill and colour lab for both",
    "bar and non-bar geom with color mappings"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(carat) as x,
				count(*) as y,
				cut as color
			from diamonds
			group by
				bin(carat),
				cut
			using bars

			layer

			visualize
				carat as x,
				price as y,
				clarity as color
			from diamonds
			using points
		")

    actual_labs <- ggplot_labs(rgs)

    expect_equal(
      actual_labs$fill,
      "cut"
    )
    expect_equal(
      actual_labs$colour,
      "clarity"
    )
  }
)

test_that("ggplot_labs returns first instances of colour and fill", {
  rgs <- sgl_to_rgs("
		visualize
			bin(carat) as x,
			count(*) as y,
			cut as color
		from diamonds
		group by
			bin(carat),
			cut
		using bars

		layer

		visualize
			bin(carat) as x,
			count(*) as y,
			clarity as color
		from diamonds
		group by
			bin(carat),
		  clarity
		using bars

		layer

		visualize
			carat as x,
			price as y,
			clarity as color
		from diamonds
		using points

		layer

		visualize
			carat as x,
			price as y,
			cut as color
		from diamonds
		using points
	")

  actual_labs <- ggplot_labs(rgs)

  expect_equal(
    actual_labs$fill,
    "cut"
  )
  expect_equal(
    actual_labs$colour,
    "clarity"
  )
})

test_that("ggplot_labs returns labs for multiple aes", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using line

		layer

		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using points

		title
			y as 'Miles Per Gallon'
	")

  actual_labs <- ggplot_labs(rgs)

  expect_equal(
    names(actual_labs),
    c("y", "x", "colour")
  )
  expect_equal(
    actual_labs$x,
    "hp"
  )
  expect_equal(
    actual_labs$y,
    "Miles Per Gallon"
  )
  expect_equal(
    actual_labs$colour,
    "cyl"
  )
})
