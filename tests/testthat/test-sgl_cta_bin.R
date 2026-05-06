test_that("new_sgl_cta_bin returns an instance of the sgl_cta_bin subclass", {
  bin <- new_sgl_cta_bin()
  expect_equal(attr(bin, "class"), c("sgl_cta_bin", "sgl_cta"))
})

test_that("new_sgl_cta_bin only adds class attribute", {
  bin <- new_sgl_cta_bin()
  all_attributes <- names(attributes(bin))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_cta_bin sets empty list as base object", {
  base_object <- unclass(new_sgl_cta_bin())

  expect_equal(base_object, list())
})

test_that("valid_cta raises error for *", {
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "*",
    cta = bin_cta
  )
  expect_error(
    valid_cta(bin_cta, col_expr, data.frame()),
    "Error: '*' can only be used inside an aggregation function",
    fixed = TRUE
  )
})

test_that("valid_cta doesn't raise error for numerical column", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "number",
    cta = bin_cta
  )

  expect_no_error(
    valid_cta(bin_cta, col_expr, df)
  )
})

test_that("valid_cta doesn't raise error for temporal column", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "temporal",
    cta = bin_cta
  )

  expect_no_error(
    valid_cta(bin_cta, col_expr, df)
  )
})

test_that("valid_cta raises error for categorical column", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "letter",
    cta = bin_cta
  )

  expect_error(
    valid_cta(bin_cta, col_expr, df),
    "Error: cannot apply bin to a categorical column, found bin(letter).",
    fixed = TRUE
  )
})

test_that("valid_cta doesn't raise error no arg", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "number",
    cta = bin_cta
  )

  expect_no_error(
    valid_cta(bin_cta, col_expr, df)
  )
})

test_that("valid_cta doesn't raise error for arg value > 0", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "number",
    cta = bin_cta,
    arg = 1
  )

  expect_no_error(
    valid_cta(bin_cta, col_expr, df)
  )
})

test_that("valid_cta raises error for arg value of 0", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "number",
    cta = bin_cta,
    arg = 0
  )

  expect_error(
    valid_cta(bin_cta, col_expr, df),
    "Error: number of bins must be greater than 0.",
    fixed = TRUE
  )
})

test_that("valid_cta raises error for negative arg value", {
  df <- DBI::dbGetQuery(test_con, "select * from synth")
  bin_cta <- new_sgl_cta_bin()
  col_expr <- list(
    column = "number",
    cta = bin_cta,
    arg = -1
  )

  expect_error(
    valid_cta(bin_cta, col_expr, df),
    "Error: number of bins must be greater than 0.",
    fixed = TRUE
  )
})

test_that(
  "add_transformed_column adds column with correct values for linear scale",
  {
    input_data <- 0:10
    df <- data.frame(col_1 = input_data)
    bin_cta <- new_sgl_cta_bin()
    scale <- new_sgl_scale_linear()
    num_bins <- 5

    expected_bin_values <- c(
      0.992, 0.992, 2.996, 2.996, 5, 5, 5, 7.004, 7.004, 9.008, 9.008
    )
    df <- add_transformed_column(
      bin_cta, "col_1", df,
      num_bins = num_bins,
      scale = scale
    )
    expect_equal(
      df$rsgl.linear.bin.5.col_1,
      expected_bin_values
    )
  }
)

test_that(
  "add_transformed_column adds column with correct values for log scale",
  {
    input_data <- 10^(0:10)
    df <- data.frame(col_1 = input_data)
    bin_cta <- new_sgl_cta_bin()
    scale <- new_sgl_scale_log()
    num_bins <- 5

    expected_bin_values <- 10^(
      c(0.992, 0.992, 2.996, 2.996, 5, 5, 5, 7.004, 7.004, 9.008, 9.008)
    )
    df <- add_transformed_column(
      bin_cta, "col_1", df,
      num_bins = num_bins,
      scale = scale
    )
    expect_equal(
      df$rsgl.log.bin.5.col_1,
      expected_bin_values
    )
  }
)

test_that(
  "add_transformed_column uses 30 bins by default",
  {
    input_data <- 0:100
    df <- data.frame(col_1 = input_data)
    bin_cta <- new_sgl_cta_bin()
    scale <- new_sgl_scale_linear()

    df <- add_transformed_column(bin_cta, "col_1", df, scale = scale)
    expect_equal(
      length(unique(df$rsgl.linear.bin.30.col_1)),
      30
    )
  }
)

test_that("add_transformed_column doesn't modify column if already exists", {
  existing_col_values <- 0
  df <- data.frame(rsgl.linear.bin.30.col_1 = existing_col_values)
  bin_cta <- new_sgl_cta_bin()
  scale <- new_sgl_scale_linear()

  df <- add_transformed_column(bin_cta, "col_1", df, scale = scale)
  expect_equal(
    df$rsgl.linear.bin.30.col_1,
    existing_col_values
  )
})

test_that("add_transformed_column raises error if dot args not provided", {
  df <- data.frame()
  bin_cta <- new_sgl_cta_bin()

  expect_error(
    add_transformed_column(bin_cta, "col_1", df),
    "Error: expected argument 'scale' to be provided.",
    fixed = TRUE
  )
})

test_that(
  paste(
    "add_transformed_column raises error if",
    "dots provided without scale keyword arg"
  ),
  {
    df <- data.frame()
    bin_cta <- new_sgl_cta_bin()

    expect_error(
      add_transformed_column(
        bin_cta, "col_1", df, "some_arg",
        another = "another_arg"
      ),
      "Error: expected argument 'scale' to be provided.",
      fixed = TRUE
    )
  }
)

test_that("is_aggregation returns false", {
  bin_cta <- new_sgl_cta_bin()

  expect_false(is_aggregation(bin_cta))
})

test_that("is_transformation returns true", {
  bin_cta <- new_sgl_cta_bin()

  expect_true(is_transformation(bin_cta))
})

describe("default_title", {
  describe("no arg for number of bins", {
    it("gives expr for bin call on column", {
      bin_cta <- new_sgl_cta_bin()
      col_expr <- list(
        column = "col_1",
        cta = bin_cta
      )

      expect_equal(
        default_title(bin_cta, col_expr),
        "bin(col_1)"
      )
    })
  })
  describe("arg for number of bins", {
    it("gives expr for bin call on column with arg", {
      bin_cta <- new_sgl_cta_bin()
      col_expr <- list(
        column = "col_1",
        cta = bin_cta,
        arg = 10
      )

      expect_equal(
        default_title(bin_cta, col_expr),
        "bin(col_1, 10)"
      )
    })
  })
})
