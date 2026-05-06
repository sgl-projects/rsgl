test_ln <- new_sgl_scale_ln()

test_that(
  paste(
    "new_sgl_scale_ln returns an instance",
    "of the sgl_scale_ln subclass"
  ),
  {
    expect_equal(attr(test_ln, "class"), c("sgl_scale_ln", "sgl_scale"))
  }
)

test_that("new_sgl_scale_ln only adds class attribute", {
  all_attributes <- names(attributes(test_ln))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_scale_ln sets empty list as base object", {
  base_object <- unclass(test_ln)

  expect_equal(base_object, list())
})

test_that("scale_name returns ln", {
  expect_equal(scale_name(test_ln), "ln")
})

test_that("valid_scale raises error if aes not in any layer", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_scale(test_ln, "color", rgs$layers, dfs),
    "Error: a scaled aesthetic must have at least one mapping",
    fixed = TRUE
  )
})

describe("valid_scale", {
  describe("single layer", {
    it("raises error if no mapping for aes", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x,
          mpg as y
        from cars
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_error(
        valid_scale(test_ln, "color", rgs$layers, dfs),
        "Error: a scaled aesthetic must have at least one mapping",
        fixed = TRUE
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

      expected_msg <- paste(
        "Error: the ln scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        valid_scale(test_ln, "x", rgs$layers, dfs),
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

      expected_msg <- paste(
        "Error: the ln scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        valid_scale(test_ln, "x", rgs$layers, dfs),
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

      expect_no_error(
        valid_scale(test_ln, "x", rgs$layers, dfs)
      )
    })
  })
  describe("multiple layers", {
    it("raises error if no mapping for aes in any layer", {
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

      expect_error(
        valid_scale(test_ln, "color", rgs$layers, dfs),
        "Error: a scaled aesthetic must have at least one mapping",
        fixed = TRUE
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

      expected_msg <- paste(
        "Error: the ln scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        valid_scale(test_ln, "x", rgs$layers, dfs),
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

      expect_no_error(
        valid_scale(test_ln, "x", rgs$layers, dfs)
      )
    })
    it("doesn't raise error due to some layers not having mapping for aes", {
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
        valid_scale(test_ln, "color", rgs$layers, dfs),
      )
    })
  })
})

test_that("apply_scale returns ln of the input values", {
  input <- c(10, NA, 27.5)

  actual <- apply_scale(test_ln, input)

  expected <- c(2.302585093, NA, 3.314186005)
  expect_equal(actual, expected)
})

test_that("apply_scale_inverse returns e to the power of the input values", {
  input <- c(2.302585093, NA, 3.314186005)

  actual <- apply_scale_inverse(test_ln, input)

  expected <- c(10, NA, 27.5)
  expect_equal(actual, expected)
})

non_color_scale_cases <- function() {
  tibble::tribble(
    ~.test_name, ~aes, ~expected_scale,
    "x", "x", list(ggplot2::scale_x_continuous(transform = "log")),
    "y", "y", list(ggplot2::scale_y_continuous(transform = "log")),
    "theta", "theta", list(ggplot2::scale_x_continuous(transform = "log")),
    "r", "r", list(ggplot2::scale_y_continuous(transform = "log")),
    "size", "size", list(ggplot2::scale_size(transform = "log"))
  )
}
patrick::with_parameters_test_that(
  "ggplot_scales returns correct scale for non-color aes:",
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
      ggplot_scales(test_ln, aes, rgs),
      expected_scale
    )
  },
  .cases = non_color_scale_cases()
)

test_that("ggplot_scales returns colour scale for non-bar with color aes", {
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
    ggplot_scales(test_ln, "color", rgs),
    list(ggplot2::scale_colour_continuous(transform = "log"))
  )
})

test_that("ggplot_scales returns fill scale for bar with color aes", {
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
    ggplot_scales(test_ln, "color", rgs),
    list(ggplot2::scale_fill_continuous(transform = "log"))
  )
})

test_that(
  paste(
    "ggplot_scales returns colour and fill scales",
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

    expect_equal(
      ggplot_scales(test_ln, "color", rgs),
      list(
        ggplot2::scale_colour_continuous(transform = "log"),
        ggplot2::scale_fill_continuous(transform = "log")
      )
    )
  }
)

test_that("ggplot_scales doesn't add duplicates for color", {
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
    ggplot_scales(test_ln, "color", rgs),
    list(ggplot2::scale_colour_continuous(transform = "log"))
  )
})
