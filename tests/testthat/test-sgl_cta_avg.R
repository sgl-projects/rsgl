test_avg <- new_sgl_cta_avg()

test_that(
  paste(
    "new_sgl_cta_avg returns an instance",
    "of the sgl_cta_avg subclass"
  ),
  {
    expect_equal(attr(test_avg, "class"), c("sgl_cta_avg", "sgl_cta"))
  }
)

test_that("new_sgl_cta_avg only adds class attribute", {
  all_attributes <- names(attributes(test_avg))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_cta_avg sets empty list as base object", {
  base_object <- unclass(test_avg)

  expect_equal(base_object, list())
})

test_that("cta_name returns avg", {
  expect_equal(cta_name(test_avg), "avg")
})

test_that("valid_cta raises error for *", {
  col_expr <- list(
    column = "*",
    cta = test_avg
  )
  expect_error(
    valid_cta(test_avg, col_expr, data.frame()),
    "Error: '*' cannot be used with the avg function.",
    fixed = TRUE
  )
})

test_that("valid_cta doesn't raise error for numerical column", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  col_expr <- list(
    column = "number",
    cta = test_avg
  )

  expect_no_error(
    valid_cta(test_avg, col_expr, df)
  )
})

test_that("valid_cta raises error for temporal column", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  col_expr <- list(
    column = "day",
    cta = test_avg
  )

  expect_error(
    valid_cta(test_avg, col_expr, df),
    "Error: avg function can only be applied to numerical columns.",
    fixed = TRUE
  )
})

test_that("valid_cta raises error for categorical column", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  col_expr <- list(
    column = "letter",
    cta = test_avg
  )

  expect_error(
    valid_cta(test_avg, col_expr, df),
    "Error: avg function can only be applied to numerical columns.",
    fixed = TRUE
  )
})

test_that("valid_cta doesn't raise error for no arg", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  col_expr <- list(
    column = "number",
    cta = test_avg
  )

  expect_no_error(
    valid_cta(test_avg, col_expr, df)
  )
})

test_that("valid_cta raises error for arg", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  col_expr <- list(
    column = "number",
    cta = test_avg,
    arg = 1
  )

  expect_error(
    valid_cta(test_avg, col_expr, df),
    "Error: avg function received unexpected argument.",
    fixed = TRUE
  )
})

describe("is_aggregation", {
  it("returns true", {
    expect_true(is_aggregation(test_avg))
  })
})

describe("is_transformation", {
  it("returns false", {
    expect_false(is_transformation(test_avg))
  })
})

describe("agg_col_name", {
  describe("scale is NULL", {
    it("returns correct name", {
      col_expr <- list(
        column = "col_1",
        cta = test_avg
      )
      result <- agg_col_name(test_avg, col_expr, NULL)
      expect_equal(
        result,
        "rsgl.linear.avg.col_1"
      )
    })
  })
  describe("scale is not NULL", {
    it("returns correct name", {
      col_expr <- list(
        column = "col_1",
        cta = test_avg
      )
      result <- agg_col_name(test_avg, col_expr, new_sgl_scale_log())
      expect_equal(
        result,
        "rsgl.log.avg.col_1"
      )
    })
  })
})

describe("agg_col_expr", {
  describe("scale is NULL", {
    it("returns correct expression", {
      col_expr <- list(
        column = "col_1",
        cta = test_avg
      )
      result <- agg_col_expr(test_avg, col_expr, NULL)
      expect_equal(
        result,
        rlang::expr(mean(col_1))
      )
    })
  })
  describe("scale is not NULL", {
    it("returns correct expression", {
      col_expr <- list(
        column = "col_1",
        cta = test_avg
      )
      result <- agg_col_expr(test_avg, col_expr, new_sgl_scale_log())
      expect_equal(
        result,
        rlang::expr(mean(rsgl.log.col_1))
      )
    })
  })
})

describe("default_title", {
  it("returns expr for avg called on column", {
    col_expr <- list(
      column = "col_1",
      cta = test_avg
    )

    expect_equal(
      default_title(test_avg, col_expr),
      "avg(col_1)"
    )
  })
})
