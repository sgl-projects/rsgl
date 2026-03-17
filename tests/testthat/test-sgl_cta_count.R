test_count <- new_sgl_cta_count()

test_that(
  paste(
    "new_sgl_cta_count returns an instance",
    "of the sgl_cta_count subclass"
  ),
  {
    expect_equal(attr(test_count, "class"), c("sgl_cta_count", "sgl_cta"))
  }
)

test_that("new_sgl_cta_count only adds class attribute", {
  all_attributes <- names(attributes(test_count))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_cta_count sets empty list as base object", {
  base_object <- unclass(test_count)

  expect_equal(base_object, list())
})

test_that("cta_name returns count", {
  expect_equal(cta_name(test_count), "count")
})

test_that("valid_cta doesn't raise error for count(*)", {
  expect_no_error(
    valid_cta(test_count, "*", data.frame())
  )
})

test_that("valid_cta raises error for non-star column", {
  expect_error(
    valid_cta(test_count, "col_1", data.frame()),
    "Error: count can only be applied to *, found count(col_1).",
    fixed = TRUE
  )
})

test_that("is_transformation returns false", {
  expect_false(is_transformation(test_count))
})
