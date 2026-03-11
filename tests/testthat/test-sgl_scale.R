test_that("new_sgl_scale returns an instance of the sgl_scale class", {
  scale <- new_sgl_scale()
  expect_equal(attr(scale, "class"), "sgl_scale")
})

test_that("new_sgl_scale only adds class attribute", {
  scale <- new_sgl_scale()
  all_attributes <- names(attributes(scale))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_scale sets empty list as base object", {
  base_object <- unclass(new_sgl_scale())

  expect_equal(base_object, list())
})

test_that("new_sgl_scale adds provided subclass", {
  scale <- new_sgl_scale("sgl_scale_xyz")
  expect_equal(attr(scale, "class"), c("sgl_scale_xyz", "sgl_scale"))
})

test_that("new_sgl_scale only adds class attribute for subclasses", {
  scale <- new_sgl_scale("sgl_scale_xyz")
  all_attributes <- names(attributes(scale))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_scale sets empty list as base object for subclass", {
  base_object <- unclass(new_sgl_scale("sgl_scale_xyz"))

  expect_equal(base_object, list())
})

test_that("scale_name returns base", {
  scale <- new_sgl_scale()
  expect_equal(scale_name(scale), "base")
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

  scale <- new_sgl_scale()
  expect_error(
    valid_scale(scale, "color", rgs$layers, dfs),
    "Error: a scaled aesthetic must have at least one mapping",
    fixed = TRUE
  )
})
