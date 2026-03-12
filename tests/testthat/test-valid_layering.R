test_that("single layer is valid", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x,
			mpg as y
		from cars
		using points
	")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_layering(rgs, dfs)
  )
})

compatible_type_cases <- function() {
  tibble::tribble(
    ~.test_name, ~layer_1_source, ~layer_1_expr, ~layer_2_source, ~layer_2_expr,
    "both numerical", "cars", "hp", "synth", "number",
    "numerical and count", "cars", "hp", "synth", "count(*)",
    "both categorical", "diamonds", "color", "synth", "letter",
    "both dates", "synth", "day", "economics", "date",
    "bin of same type", "cars", "hp", "cars", "bin(mpg)"
  )
}
patrick::with_parameters_test_that(
  "layering of compatible types is valid:",
  {
    sgl <- sprintf(
      "
				visualize
					%s as x
				from %s
				using points

				layer

				visualize
					%s as x
				from %s
				using points
			",
      layer_1_expr,
      layer_1_source,
      layer_2_expr,
      layer_2_source
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_layering(rgs, dfs)
    )
  },
  .cases = compatible_type_cases()
)

test_that("layering of compatible types is valid: both datetimes", {
  DBI::dbBegin(test_con)
  withr::defer(DBI::dbRollback(test_con))
  DBI::dbExecute(
    test_con,
    "alter table economics add column day_and_time timestamp"
  )
  DBI::dbExecute(
    test_con,
    "update economics set day_and_time = date + interval '1 day'"
  )
  rgs <- sgl_to_rgs("
		visualize
			day_and_time as x
		from synth
		using points

		layer

		visualize
			day_and_time as x
		from economics
		using points
	")
  dfs <- result_dfs(rgs, test_con)

  expect_no_error(
    valid_layering(rgs, dfs)
  )
})

incompatible_type_cases <- function() {
  tibble::tribble(
    ~.test_name, ~layer_1_source, ~layer_1_expr, ~layer_2_source, ~layer_2_expr,
    "num and cat", "cars", "hp", "synth", "letter",
    "num and temp", "cars", "hp", "synth", "day",
    "cat and temp", "synth", "letter", "synth", "day_and_time",
    "date and datetime", "synth", "day", "synth", "day_and_time"
  )
}
patrick::with_parameters_test_that(
  "layering of incompatible types is invalid:",
  {
    sgl <- sprintf(
      "
				visualize
					%s as x
				from %s
				using points

				layer

				visualize
					%s as x
				from %s
				using points
			",
      layer_1_expr,
      layer_1_source,
      layer_2_expr,
      layer_2_source
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_error(
      valid_layering(rgs, dfs)
    )
  },
  .cases = incompatible_type_cases()
)

test_that("incompatible type layering gives correct error message", {
  rgs <- sgl_to_rgs("
		visualize
			hp as x
		from cars
		using points

		layer

		visualize
			letter as x
		from synth
		using points
	")
  dfs <- result_dfs(rgs, test_con)

  expected_msg <- paste(
    "Error: an aesthetic must be mapped to the same type",
    "(numerical, categorical, or temporal) across layers.",
    "Found the following types for the x aesthetic: numerical, categorical."
  )
  expect_error(
    valid_layering(rgs, dfs),
    expected_msg,
    fixed = TRUE
  )
})

patrick::with_parameters_test_that(
  "no error is raised for valid layering of other aesthetics:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as %s
				from cars
				using points

				layer

				visualize
					number as %s
				from synth
				using points
			",
      aes,
      aes
    )

    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_layering(rgs, dfs)
    )
  },
  aes = c("y", "theta", "r", "color"),
  .test_name = aes
)

patrick::with_parameters_test_that(
  "error is raised for invalid layering of other aesthetics:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as %s
				from cars
				using points

				layer

				visualize
					letter as %s
				from synth
				using points
			",
      aes,
      aes
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_error(
      valid_layering(rgs, dfs)
    )
  },
  aes = c("y", "theta", "r", "color"),
  .test_name = aes
)

patrick::with_parameters_test_that(
  "no error is raised for non-positional aesthetic in one layer only:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as x,
					mpg as y,
					cyl as %s
				from cars
				using points

				layer

				visualize
					pop as x,
					umemploy as y
				from economics
				using points
			",
      aes
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_layering(rgs, dfs)
    )
  },
  aes = c("color", "size"),
  .test_name = aes
)

patrick::with_parameters_test_that(
  "no error is raised for consistent coordinates across all layers:",
  {
    sgl <- sprintf(
      "
				visualize
					hp as %s,
					mpg as %s,
					cyl as color
				from cars
				using points

				layer

				visualize
					hp as %s,
					mpg as %s
				from cars
				using regression line
			",
      coords[1],
      coords[2],
      coords[1],
      coords[2]
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expect_no_error(
      valid_layering(rgs, dfs)
    )
  },
  coords = list(.cart_aes, .polar_aes),
  .test_name = c("cartesian", "polar")
)

patrick::with_parameters_test_that(
  "error is raised if positional aesthetics are inconsistent across layers:",
  {
    different_aes <- .pos_aes[.pos_aes != test_aes][1]
    sgl <- sprintf(
      "
				visualize
					hp as %s
				from cars
				using points

				layer

				visualize
					mpg as %s
				from cars
				using points
			",
      different_aes,
      test_aes
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)

    expected_errmsg <- sprintf(
      paste(
        "Error: if a positional aesthetic is present in one layer,",
        "it must be present in all layers. '%s' is not present in all layers."
      ),
      test_aes
    )
    expect_error(
      valid_layering(rgs, dfs),
      expected_errmsg,
      fixed = TRUE
    )
  },
  test_aes = .pos_aes,
  .test_name = test_aes
)
