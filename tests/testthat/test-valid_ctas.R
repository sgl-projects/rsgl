valid_cta_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "identity", "hp",
    "bin", "bin(mpg)",
    "count", "count(*)"
  )
}
valid_cta_test_body <- function(sgl) {
  rgs <- sgl_to_rgs(sgl)
  dfs <- result_dfs(rgs, test_con) # nolint: object_usage_linter
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error( # nolint: object_usage_linter
    valid_ctas(layer, df)
  )
}
patrick::with_parameters_test_that(
  "valid_ctas doesn't raise error for valid cta's in aes mapping:",
  {
    sgl <- sprintf(
      "
				visualize
					%s as x
				from cars
				using points
			",
      expr
    )
    valid_cta_test_body(sgl)
  },
  .cases = valid_cta_cases()
)
patrick::with_parameters_test_that(
  "valid_ctas doesn't raise error for valid cta's in group by clause:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as x
				from cars
				group by
					%s
				using points
			",
      expr
    )
    valid_cta_test_body(sgl)
  },
  .cases = valid_cta_cases()
)
patrick::with_parameters_test_that(
  "valid_ctas doesn't raise error for valid cta's in collect by clause:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as x
				from cars
				collect by
					%s
				using points
			",
      expr
    )
    valid_cta_test_body(sgl)
  },
  .cases = valid_cta_cases()
)

invalid_cta_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "identity", "*",
    "bin", "bin(cut)",
    "count", "count(cut)"
  )
}
invalid_cta_test_body <- function(sgl) {
  rgs <- sgl_to_rgs(sgl)
  dfs <- result_dfs(rgs, test_con) # nolint: object_usage_linter
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error( # nolint: object_usage_linter
    valid_ctas(layer, df)
  )
}
patrick::with_parameters_test_that(
  "valid_ctas raises error for invalid cta's in aes mapping:",
  {
    sgl <- sprintf(
      "
				visualize
					%s as x
				from diamonds
				using points
			",
      expr
    )
    invalid_cta_test_body(sgl)
  },
  .cases = invalid_cta_cases()
)
patrick::with_parameters_test_that(
  "valid_ctas raises error for invalid cta's in group by clause:",
  {
    sgl <- sprintf(
      "
				visualize
					price as x
				from diamonds
				group by
					%s
				using points
			",
      expr
    )
    invalid_cta_test_body(sgl)
  },
  .cases = invalid_cta_cases()
)
patrick::with_parameters_test_that(
  "valid_ctas raises error for invalid cta's in collect by clause:",
  {
    sgl <- sprintf(
      "
				visualize
					price as x
				from diamonds
				collect by
					%s
				using points
			",
      expr
    )
    invalid_cta_test_body(sgl)
  },
  .cases = invalid_cta_cases()
)

test_that(
  "valid_ctas doesn't raise error for valid cta's across multiple clauses",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(carat) as x,
				count(*) as y,
				cut as color
			from cars
			group by
				bin(carat),
				cut
			collect by
				cut
			using lines
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_ctas(layer, df)
    )
  }
)
