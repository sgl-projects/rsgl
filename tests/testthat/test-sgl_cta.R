test_that("new_sgl_cta returns an instance of the sgl_cta class", {
  cta <- new_sgl_cta()
  expect_equal(attr(cta, "class"), "sgl_cta")
})

test_that("new_sgl_cta only adds class attribute", {
  cta <- new_sgl_cta()
  all_attributes <- names(attributes(cta))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_cta sets empty list as base object", {
  base_object <- unclass(new_sgl_cta())

  expect_equal(base_object, list())
})

test_that("new_sgl_cta adds provided subclass", {
  cta <- new_sgl_cta("sgl_cta_xyz")
  expect_equal(attr(cta, "class"), c("sgl_cta_xyz", "sgl_cta"))
})

test_that("new_sgl_cta only adds class attribute for subclasses", {
  cta <- new_sgl_cta("sgl_cta_xyz")
  all_attributes <- names(attributes(cta))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_cta sets empty list as base object for subclass", {
  base_object <- unclass(new_sgl_cta("sgl_cta_xyz"))

  expect_equal(base_object, list())
})
