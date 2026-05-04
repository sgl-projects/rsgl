col_expr_has_cta_cases <- function() {
  tibble::tribble(
    ~.test_name, ~col_expr_cta, ~fn_arg_cta_name, ~expected_result,
    "id expr has id", new_sgl_cta_identity(), "identity", TRUE,
    "bin expr does not have id", new_sgl_cta_bin(), "identity", FALSE,
    "count expr does not id", new_sgl_cta_count(), "identity", FALSE,
    "avg expr does not have id", new_sgl_cta_avg(), "identity", FALSE,
    "id expr does not have bin", new_sgl_cta_identity(), "bin", FALSE,
    "bin expr has bin", new_sgl_cta_bin(), "bin", TRUE,
    "count expr does not have bin", new_sgl_cta_count(), "bin", FALSE,
    "avg expr does not have bin", new_sgl_cta_avg(), "bin", FALSE,
    "id expr does not have count", new_sgl_cta_identity(), "count", FALSE,
    "bin expr does not have count", new_sgl_cta_bin(), "count", FALSE,
    "count expr has count", new_sgl_cta_count(), "count", TRUE,
    "avg expr does not have count", new_sgl_cta_avg(), "count", FALSE,
    "id expr does not have avg", new_sgl_cta_identity(), "avg", FALSE,
    "bin expr does not have avg", new_sgl_cta_bin(), "avg", FALSE,
    "count expr does not have avg", new_sgl_cta_count(), "avg", FALSE,
    "avg expr has avg", new_sgl_cta_avg(), "avg", TRUE
  )
}
patrick::with_parameters_test_that(
  "col_expr_has_cta:",
  {
    col_expr <- list(
      column = "col_1",
      cta = col_expr_cta
    )

    expect_equal(
      col_expr_has_cta(col_expr, fn_arg_cta_name),
      expected_result
    )
  },
  .cases = col_expr_has_cta_cases()
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

describe("filter_agg_exprs", {
  describe("no col_exprs have aggregation", {
    it("returns an empty list", {
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

      expect_equal(
        filter_agg_exprs(col_exprs),
        list()
      )
    })
  })
  describe("some col_exprs have aggregation", {
    it("returns agg exprs only", {
      col_expr_1 <- list(
        column = "col_1",
        cta = new_sgl_cta_identity()
      )
      col_expr_2 <- list(
        column = "col_2",
        cta = new_sgl_cta_count()
      )
      col_expr_3 <- list(
        column = "col_3",
        cta = new_sgl_cta_avg()
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
        filter_agg_exprs(col_exprs),
        expected_result
      )
    })
  })
})
