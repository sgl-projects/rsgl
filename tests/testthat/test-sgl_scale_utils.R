test_that("non_numerical_in_layer returns FALSE for no aes mapping", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x
		from cars
		using points
	")
  layer <- rgs$layers[[1]]
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]

  expect_false(
    non_numerical_in_layer("y", layer, df)
  )
})

test_that("non_numerical_in_layer returns FALSE for numerical mapping", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x
		from cars
		using points
	")
  layer <- rgs$layers[[1]]
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]

  expect_false(
    non_numerical_in_layer("x", layer, df)
  )
})

test_that("non_numerical_in_layer returns TRUE for categorical mapping", {
  rgs <- sgl_to_rgs("
		visualize
			cut as x
		from diamonds
		using points
	")
  layer <- rgs$layers[[1]]
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]

  expect_true(
    non_numerical_in_layer("x", layer, df)
  )
})

test_that("non_numerical_in_layer returns TRUE for temporal mapping", {
  rgs <- sgl_to_rgs("
		visualize
			day as x
		from synth
		using points
	")
  layer <- rgs$layers[[1]]
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]

  expect_true(
    non_numerical_in_layer("x", layer, df)
  )
})

describe("raise_for_non_nums", {
  describe("single layer", {
    it("doesn't raise error if no mapping for aes", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x,
          mpg as y
        from cars
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expect_no_error(
        raise_for_non_nums(scale, "color", rgs$layers, dfs),
      )
    })
    it("raises error if mapping for aes is categorical", {
      rgs <- sgl_to_rgs("
        visualize
          cut as x
        from diamonds
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expected_msg <- paste(
        "Error: the base scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        raise_for_non_nums(scale, "x", rgs$layers, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
    it("raises error if mapping for aes is temporal", {
      rgs <- sgl_to_rgs("
        visualize
          day as x
        from synth
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expected_msg <- paste(
        "Error: the base scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        raise_for_non_nums(scale, "x", rgs$layers, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
    it("doesn't raise error if mapping for aes is numerical", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x
        from cars
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expect_no_error(
        raise_for_non_nums(scale, "x", rgs$layers, dfs)
      )
    })
  })
  describe("multiple layers", {
    it("doesn't raise error if no mapping for aes in any layer", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x,
          mpg as y
        from cars
        using (
          points
          layer
          regression line
        )
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expect_no_error(
        raise_for_non_nums(scale, "color", rgs$layers, dfs)
      )
    })
    it("raises error if any mapping for aes is non-numerical", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x
        from cars
        using points

        layer

        visualize
          day as x
        from synth
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expected_msg <- paste(
        "Error: the base scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        raise_for_non_nums(scale, "x", rgs$layers, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
    it("doesn't raise error if all mappings for aes are numerical", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x
        from cars
        using points

        layer

        visualize
          number as x
        from synth
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      scale <- new_sgl_scale()
      expect_no_error(
        raise_for_non_nums(scale, "x", rgs$layers, dfs)
      )
    })
  })
})

test_that("ggplot_color_aes returns empty vec if no color aes specified", {
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
		using bars
	")

  expect_equal(
    ggplot_color_aes(rgs),
    character(0)
  )
})

test_that("ggplot_color_aes returns colour for non-bar with color aes", {
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
		using bars
	")

  expect_equal(
    ggplot_color_aes(rgs),
    "colour"
  )
})

test_that("ggplot_color_aes returns fill for bar with color aes", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using points

		layer

		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using bars
	")

  expect_equal(
    ggplot_color_aes(rgs),
    "fill"
  )
})

test_that(
  paste(
    "ggplot_color_aes returns colour and fill",
    "for both and non-bar with color aes"
  ),
  {
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
				mpg as y,
				cyl as color
			from cars
			using bars
		")

    expect_setequal(
      ggplot_color_aes(rgs),
      c("colour", "fill")
    )
  }
)


test_that("ggplot_color_aes doesn't add duplicates", {
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
			mpg as y,
			cyl as color
		from cars
		using line
	")

  expect_equal(
    ggplot_color_aes(rgs),
    "colour"
  )
})

non_color_scale_cases <- function() {
  tibble::tribble(
    ~.test_name, ~aes, ~expected_scale,
    "x", "x", list(ggplot2::scale_x_continuous(transform = "identity")),
    "y", "y", list(ggplot2::scale_y_continuous(transform = "identity")),
    "theta", "theta", list(ggplot2::scale_x_continuous(transform = "identity")),
    "r", "r", list(ggplot2::scale_y_continuous(transform = "identity")),
    "size", "size", list(ggplot2::scale_size(transform = "identity"))
  )
}
patrick::with_parameters_test_that(
  "ggplot_continuous_scales returns correct scale for non-color aes:",
  {
    sgl <- sprintf(
      "
        visualize
          hp as %s
        from cars
        using points
      ",
      aes
    )
    rgs <- sgl_to_rgs(sgl)

    expect_equal(
      ggplot_continuous_scales("identity", aes, rgs),
      expected_scale
    )
  },
  .cases = non_color_scale_cases()
)

test_that(
  paste(
    "ggplot_continuous_scales returns colour",
    "scale for non-bar with color aes"
  ),
  {
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
			using bars
		")

    expect_equal(
      ggplot_continuous_scales("identity", "color", rgs),
      list(ggplot2::scale_colour_continuous(transform = "identity"))
    )
  }
)

test_that(
  paste(
    "ggplot_continuous_scales returns fill",
    "scale for bar with color aes"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points

			layer

			visualize
				hp as x,
				mpg as y,
				cyl as color
			from cars
			using bars
		")

    expect_equal(
      ggplot_continuous_scales("identity", "color", rgs),
      list(ggplot2::scale_fill_continuous(transform = "identity"))
    )
  }
)

test_that(
  paste(
    "ggplot_continuous_scales returns colour and fill scales",
    "for both bar and non-bar with color aes"
  ),
  {
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
        mpg as y,
        cyl as color
      from cars
      using bars
    ")

    expect_setequal(
      ggplot_continuous_scales("identity", "color", rgs),
      list(
        ggplot2::scale_colour_continuous(transform = "identity"),
        ggplot2::scale_fill_continuous(transform = "identity")
      )
    )
  }
)

test_that("ggplot_continuous_scales doesn't add duplicates for color", {
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
      mpg as y,
      cyl as color
    from cars
    using line
  ")

  expect_equal(
    ggplot_continuous_scales("identity", "color", rgs),
    list(ggplot2::scale_colour_continuous(transform = "identity"))
  )
})

non_identity_scale_cases <- function() {
  tibble::tribble(
    ~.test_name, ~aes, ~expected_scale,
    "x", "x", list(ggplot2::scale_x_continuous(transform = "log10")),
    "y", "y", list(ggplot2::scale_y_continuous(transform = "log10")),
    "theta", "theta", list(ggplot2::scale_x_continuous(transform = "log10")),
    "r", "r", list(ggplot2::scale_y_continuous(transform = "log10")),
    "color", "color", list(
      ggplot2::scale_colour_continuous(transform = "log10")
    ),
    "size", "size", list(ggplot2::scale_size(transform = "log10"))
  )
}
patrick::with_parameters_test_that(
  "ggplot_continuous_scales returns correct scale for non-identity transforms:",
  {
    sgl <- sprintf(
      "
        visualize
          hp as %s
        from cars
        using points
      ",
      aes
    )
    rgs <- sgl_to_rgs(sgl)

    expect_equal(
      ggplot_continuous_scales("log10", aes, rgs),
      expected_scale
    )
  },
  .cases = non_identity_scale_cases()
)
