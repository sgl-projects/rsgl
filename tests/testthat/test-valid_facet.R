test_that("doesn't raise error for no faceting", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for valid default faceting", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for valid horizontal faceting", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean horizontally
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for valid vertical faceting", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean vertically
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for two default facets", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean,
			letter
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for one default and one horizontal facet", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean,
			letter horizontally
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for one default and one vertical facet", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean,
			letter vertically
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("doesn't raise error for one horizontal and one vertical facet", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			boolean horizontally,
			letter vertically
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_facet(rgs, dfs)
  )
})

test_that("raises error for faceting on a column that doesn't exist", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			not_a_col
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_facet(rgs, dfs),
    "Error: facet column 'not_a_col' does not exist in any layer data sources.",
    fixed = TRUE
  )
})

facet_type_cases <- function() {
  tibble::tribble(
    ~.test_name, ~column_name,
    "numerical", "number",
    "temporal", "day_and_time",
    "categorical", "boolean",
  )
}
patrick::with_parameters_test_that(
  "doesn't raise error due to facet column type:",
  {
    sgl <- sprintf(
      "
        visualize
        	letter as x,
					day as y
        from synth
        using points
				facet by
					%s
      ",
      column_name
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_facet(rgs, dfs)
    )
  },
  .cases = facet_type_cases()
)

test_that("raises error for two horizontal facets", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			letter horizontally,
			boolean horizontally
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_facet(rgs, dfs),
    "Error: for two facets, one must be horizontal and the other vertical.",
    fixed = TRUE
  )
})

test_that("raises error for two vertical facets", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points
		facet by
			letter vertically,
			boolean vertically
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_facet(rgs, dfs),
    "Error: for two facets, one must be horizontal and the other vertical.",
    fixed = TRUE
  )
})

test_that("raises error for more than two facets", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from (
			select
				*,
				not boolean as boolean_opposite
			from synth
		)
    using points
		facet by
			letter,
			boolean,
			boolean_opposite
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_facet(rgs, dfs),
    "Error: cannot have more than two facets.",
    fixed = TRUE
  )
})

valid_multiple_layer_cases <- function() {
  tibble::tribble(
    ~.test_name, ~column_name,
    "numerical", "number",
    "temporal", "day_and_time",
    "categorical", "boolean",
  )
}
patrick::with_parameters_test_that(
  "doesn't raise error for valid faceting with multiple layers:",
  {
    sgl <- sprintf(
      "
				visualize
					letter as x,
					day as y
				from synth
				using points

				layer

				visualize
					letter as x,
					day as y
				from synth
				using line

				facet by
					%s
			",
      column_name
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_facet(rgs, dfs)
    )
  },
  .cases = valid_multiple_layer_cases()
)

inconsistent_type_cases <- function() {
  tibble::tribble(
    ~.test_name, ~first_col, ~second_col,
    "num and tmp", "number", "day_and_time",
    "num and cat", "number", "boolean",
    "tmp and cat", "day_and_time", "boolean"
  )
}
patrick::with_parameters_test_that(
  "raises error when facet column has inconsistent type across layers:",
  {
    sgl <- sprintf(
      "
				visualize
					letter as x,
					day as y
				from (
					select
						*,
						%s as facet_col
					from synth
				)
				using points

				layer

				visualize
					letter as x,
					day as y
				from (
					select
						*,
						%s as facet_col
					from synth
				)
				using points

				facet by
					facet_col
			",
      first_col,
      second_col
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expected_msg <- paste(
      "Error: facet column 'facet_col' does not have a",
      "consistent type (categorical, numerical, or temporal)",
      "across all layers where it is present."
    )
    expect_error(
      valid_facet(rgs, dfs),
      expected_msg,
      fixed = TRUE
    )
  },
  .cases = inconsistent_type_cases()
)

test_that("raises error if facet col has unknown type class", {
  DBI::dbBegin(test_con)
  withr::defer(DBI::dbRollback(test_con))
  DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")

  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using line

    facet by
      blob_col
  ")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: unknown SGL type classification",
    "(numerical, categorical, or temporal)",
    "for column 'blob_col'."
  )
  expect_error(
    valid_facet(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

test_that("raises error for invalid faceting with multiple layers", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y
    from synth
    using points

		layer

		visualize
			day as x,
			number_plus_one as y
		from (
			select
				*,
				number + 1 as number_plus_one
			from synth
		)
		using points

		facet by
			letter horizontally,
			boolean horizontally
  ")
  dfs <- result_dfs(rgs, test_con)

  expect_error(
    valid_facet(rgs, dfs),
    "Error: for two facets, one must be horizontal and the other vertical.",
    fixed = TRUE
  )
})

test_that(
  "doesn't raise error if facet column exists in some but not all layers",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				number as y
			from synth
			using points

			layer

			visualize
				day as x,
				number as y
			from (
				select
					day,
					number
				from synth
			)
			using points

			facet by
				boolean
		")
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_facet(rgs, dfs)
    )
  }
)
