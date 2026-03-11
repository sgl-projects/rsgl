identity <- new_sgl_cta_identity()

test_that(
  "new_sgl_cta_identity returns an instance of the sgl_cta_identity subclass",
  {
    expect_equal(
      attr(identity, "class"),
      c("sgl_cta_identity", "sgl_cta")
    )
  }
)

test_that("new_sgl_cta_identity only adds class attribute", {
  expect_equal(
    names(attributes(identity)),
    "class"
  )
})

test_that("new_sgl_cta_identity sets empty list as base object", {
  expect_equal(
    unclass(identity),
    list()
  )
})

test_that("valid_cta raises error for *", {
  expect_error(
    valid_cta(identity, "*", data.frame()),
    "Error: '*' can only be used inside an aggregation function",
    fixed = TRUE
  )
})

test_that("valid_cta doesn't raise error for non-* column", {
  expect_no_error(
    valid_cta(identity, "col", data.frame())
  )
})

test_that("is_transformation returns false", {
  identity_cta <- new_sgl_cta_identity()
  expect_false(is_transformation(identity_cta))
})
