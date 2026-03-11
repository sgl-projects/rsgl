test_that("col_expr_has_cta returns true for bin tran with bin arg", {
  col_expr <- list(
    column = "col_1",
    cta = new_sgl_cta_bin()
  )

  expect_true(
    col_expr_has_cta(col_expr, "bin")
  )
})

test_that("col_expr_has_cta returns false for non-bin tran with bin arg", {
  col_expr <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )

  expect_false(
    col_expr_has_cta(col_expr, "bin")
  )
})

test_that("col_expr_has_cta returns true for count tran with count arg", {
  col_expr <- list(
    column = "col_1",
    cta = new_sgl_cta_count()
  )

  expect_true(
    col_expr_has_cta(col_expr, "count")
  )
})

test_that("col_expr_has_cta returns false for non-count tran with count arg", {
  col_expr <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )

  expect_false(
    col_expr_has_cta(col_expr, "count")
  )
})

test_that("col_expr_has_cta returns true for identity tran with identity arg", {
  col_expr <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )

  expect_true(
    col_expr_has_cta(col_expr, "identity")
  )
})

test_that(
  "col_expr_has_cta returns false for non-identity tran with identity arg",
  {
    col_expr <- list(
      column = "col_1",
      cta = new_sgl_cta_count()
    )

    expect_false(
      col_expr_has_cta(col_expr, "identity")
    )
  }
)

test_that("col_expr_has_cta raises error for invalid cta_name arg", {
  col_expr <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )

  expect_error(
    col_expr_has_cta(col_expr, "notacta")
  )
})

test_that("filter_col_exprs_by_cta returns empty list for empty list input", {
  col_exprs <- list()

  expect_equal(
    filter_col_exprs_by_cta(col_exprs, "bin"),
    list()
  )
})

test_that("filter_col_exprs_by_cta returns empty list if none match", {
  col_expr_1 <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )
  col_expr_2 <- list(
    column = "col_2",
    cta = new_sgl_cta_count()
  )
  col_exprs <- list(
    col_expr_1,
    col_expr_2
  )

  expect_equal(
    filter_col_exprs_by_cta(col_exprs, "bin"),
    list()
  )
})

test_that("filter_col_exprs_by_cta returns single match", {
  col_expr_1 <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )
  col_expr_2 <- list(
    column = "col_2",
    cta = new_sgl_cta_bin()
  )
  col_exprs <- list(
    col_expr_1,
    col_expr_2
  )

  expected_result <- list(
    col_expr_2
  )

  expect_equal(
    filter_col_exprs_by_cta(col_exprs, "bin"),
    expected_result
  )
})

test_that("filter_col_exprs_by_cta returns multiple matches", {
  col_expr_1 <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )
  col_expr_2 <- list(
    column = "col_2",
    cta = new_sgl_cta_bin()
  )
  col_expr_3 <- list(
    column = "col_3",
    cta = new_sgl_cta_bin()
  )

  col_exprs <- list(
    col_expr_1,
    col_expr_2,
    col_expr_3
  )

  expected_result <- list(
    col_expr_2,
    col_expr_3
  )

  expect_equal(
    filter_col_exprs_by_cta(col_exprs, "bin"),
    expected_result
  )
})

test_that("filter_col_exprs_by_cta returns matches for cta's other than bin", {
  col_expr_1 <- list(
    column = "col_1",
    cta = new_sgl_cta_identity()
  )
  col_expr_2 <- list(
    column = "col_2",
    cta = new_sgl_cta_bin()
  )
  col_exprs <- list(
    col_expr_1,
    col_expr_2
  )

  expected_result <- list(
    col_expr_1
  )

  expect_equal(
    filter_col_exprs_by_cta(col_exprs, "identity"),
    expected_result
  )
})

test_that("filter_col_exprs_by_cta raises error for invalid cta name", {
  expect_error(
    filter_col_exprs_by_cta(list(), "notacta")
  )
})
