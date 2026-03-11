test_that(
  paste(
    "new_sgl_cta_count returns an instance",
    "of the sgl_cta_count subclass"
  ),
  {
    count <- new_sgl_cta_count()
    expect_equal(attr(count, "class"), c("sgl_cta_count", "sgl_cta"))
  }
)

test_that("new_sgl_cta_count only adds class attribute", {
  count <- new_sgl_cta_count()
  all_attributes <- names(attributes(count))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_cta_count sets empty list as base object", {
  base_object <- unclass(new_sgl_cta_count())

  expect_equal(base_object, list())
})

test_that("cta_name returns count", {
  count <- new_sgl_cta_count()
  expect_equal(cta_name(count), "count")
})

test_that("valid_cta doesn't raise error for count(*)", {
  count_cta <- new_sgl_cta_count()
  expect_no_error(
    valid_cta(count_cta, "*", data.frame())
  )
})

test_that("valid_cta raises error for non-star column", {
  count_cta <- new_sgl_cta_count()
  expect_error(
    valid_cta(count_cta, "col_1", data.frame()),
    "Error: count can only be applied to *, found count(col_1).",
    fixed = TRUE
  )
})

test_that("is_transformation returns false", {
  count_cta <- new_sgl_cta_count()
  expect_false(is_transformation(count_cta))
})
