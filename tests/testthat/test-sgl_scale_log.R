test_that(
  paste(
    "new_sgl_scale_log returns an instance",
    "of the sgl_scale_log subclass"
  ),
  {
    log <- new_sgl_scale_log()
    expect_equal(attr(log, "class"), c("sgl_scale_log", "sgl_scale"))
  }
)

test_that("new_sgl_scale_log only adds class attribute", {
  log <- new_sgl_scale_log()
  all_attributes <- names(attributes(log))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_scale_log sets empty list as base object", {
  base_object <- unclass(new_sgl_scale_log())

  expect_equal(base_object, list())
})

test_that("scale_name returns log", {
  log <- new_sgl_scale_log()
  expect_equal(scale_name(log), "log")
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

  log <- new_sgl_scale_log()
  expect_error(
    valid_scale(log, "color", rgs$layers, dfs),
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

      log <- new_sgl_scale_log()
      expect_error(
        valid_scale(log, "color", rgs$layers, dfs),
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

      log <- new_sgl_scale_log()
      expected_msg <- paste(
        "Error: the log scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        valid_scale(log, "x", rgs$layers, dfs),
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

      log <- new_sgl_scale_log()
      expected_msg <- paste(
        "Error: the log scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        valid_scale(log, "x", rgs$layers, dfs),
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

      log <- new_sgl_scale_log()
      expect_no_error(
        valid_scale(log, "x", rgs$layers, dfs)
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

      log <- new_sgl_scale_log()
      expect_error(
        valid_scale(log, "color", rgs$layers, dfs),
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

      log <- new_sgl_scale_log()
      expected_msg <- paste(
        "Error: the log scale can only be applied",
        "to aesthetics with numerical mappings."
      )
      expect_error(
        valid_scale(log, "x", rgs$layers, dfs),
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

      log <- new_sgl_scale_log()
      expect_no_error(
        valid_scale(log, "x", rgs$layers, dfs)
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

      log <- new_sgl_scale_log()
      expect_no_error(
        valid_scale(log, "color", rgs$layers, dfs),
      )
    })
  })
})

test_that("apply_scale returns log (base 10) of the input values", {
  input <- c(10, NA, 27.5)
  log <- new_sgl_scale_log()

  actual <- apply_scale(log, input)

  expected <- c(1, NA, 1.43933269)
  expect_equal(actual, expected)
})

test_that("apply_scale_inverse returns 10 to the power of the input values", {
  input <- c(1, NA, 1.43933269)
  log <- new_sgl_scale_log()

  actual <- apply_scale_inverse(log, input)

  expected <- c(10, NA, 27.5)
  expect_equal(actual, expected)
})

non_color_scale_cases <- function() {
  tibble::tribble(
    ~.test_name, ~aes, ~expected_scale,
    "x", "x", list(ggplot2::scale_x_continuous(transform = "log10")),
    "y", "y", list(ggplot2::scale_y_continuous(transform = "log10")),
    "theta", "theta", list(ggplot2::scale_x_continuous(transform = "log10")),
    "r", "r", list(ggplot2::scale_y_continuous(transform = "log10")),
    "size", "size", list(ggplot2::scale_size(transform = "log10"))
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
    log <- new_sgl_scale_log()

    expect_equal(
      ggplot_scales(log, aes, rgs),
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
  log <- new_sgl_scale_log()

  expect_equal(
    ggplot_scales(log, "color", rgs),
    list(ggplot2::scale_colour_continuous(transform = "log10"))
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
  log <- new_sgl_scale_log()

  expect_equal(
    ggplot_scales(log, "color", rgs),
    list(ggplot2::scale_fill_continuous(transform = "log10"))
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
    log <- new_sgl_scale_log()

    expect_equal(
      ggplot_scales(log, "color", rgs),
      list(
        ggplot2::scale_fill_continuous(transform = "log10"),
        ggplot2::scale_colour_continuous(transform = "log10")
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
  log <- new_sgl_scale_log()

  expect_equal(
    ggplot_scales(log, "color", rgs),
    list(ggplot2::scale_colour_continuous(transform = "log10"))
  )
})
