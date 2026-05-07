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

test_that("cta_fn_name returns count", {
  expect_equal(cta_fn_name(test_count), "count")
})

test_that("valid_cta doesn't raise error for count(*)", {
  col_expr <- list(
    column = "*",
    cta = test_count
  )

  expect_no_error(
    valid_cta(test_count, col_expr, data.frame())
  )
})

test_that("valid_cta raises error for non-star column", {
  col_expr <- list(
    column = "col_1",
    cta = test_count
  )

  expect_error(
    valid_cta(test_count, col_expr, data.frame()),
    "Error: count can only be applied to *, found count(col_1).",
    fixed = TRUE
  )
})

test_that("valid_cta doesn't raise error for no arg", {
  col_expr <- list(
    column = "*",
    cta = test_count
  )

  expect_no_error(
    valid_cta(test_count, col_expr, data.frame())
  )
})

test_that("valid_cta raises error for arg", {
  col_expr <- list(
    column = "*",
    cta = test_count,
    arg = 5
  )

  expect_error(
    valid_cta(test_count, col_expr, data.frame()),
    "Error: count function received unexpected argument.",
    fixed = TRUE
  )
})

test_that("is_aggregation returns true", {
  expect_true(is_aggregation(test_count))
})

test_that("is_transformation returns false", {
  expect_false(is_transformation(test_count))
})

describe("agg_col_name", {
  describe("scale is NULL", {
    it("returns rsgl.count", {
      result <- agg_col_name(test_count, list(), NULL)
      expect_equal(
        result,
        "rsgl.count"
      )
    })
  })
  describe("scale is not NULL", {
    it("returns rsgl.count", {
      result <- agg_col_name(test_count, list(), new_sgl_scale_log())
      expect_equal(
        result,
        "rsgl.count"
      )
    })
  })
})

describe("agg_col_expr", {
  describe("scale is NULL", {
    it("returns dplyr::n()", {
      result <- agg_col_expr(test_count, list(), NULL)
      expect_equal(
        result,
        rlang::expr(dplyr::n())
      )
    })
  })
  describe("scale is not NULL", {
    it("returns dplyr::n()", {
      result <- agg_col_expr(test_count, list(), new_sgl_scale_log())
      expect_equal(
        result,
        rlang::expr(dplyr::n())
      )
    })
  })
})

describe("expr_text", {
  it("returns count(*)", {
    expect_equal(
      expr_text(test_count, list()),
      "count(*)"
    )
  })
})
