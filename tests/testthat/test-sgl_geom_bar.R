test_that("new_sgl_geom_bar returns an instance of the sgl_geom_bar subclass", {
  bar <- new_sgl_geom_bar()
  expect_equal(attr(bar, "class"), c("sgl_geom_bar", "sgl_geom"))
})

test_that("new_sgl_geom_bar only adds class attribute", {
  bar <- new_sgl_geom_bar()
  all_attributes <- names(attributes(bar))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom_bar sets empty list as base object", {
  base_object <- unclass(new_sgl_geom_bar())

  expect_equal(base_object, list())
})

test_that("geom_name returns bar", {
  bar <- new_sgl_geom_bar()
  actual <- geom_name(bar)
  expect_equal(actual, "bar")
})

test_that("is_collective returns false", {
  bar <- new_sgl_geom_bar()
  expect_equal(is_collective(bar), FALSE)
})

test_that("ggplot_geom returns geom_bar function", {
  bar <- new_sgl_geom_bar()
  actual <- ggplot_geom(bar)
  expect_equal(actual, ggplot2::geom_bar)
})

test_that(
  paste(
    "valid_aesthetics considers categorical x",
    "and numerical y mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				number as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical y",
    "and numerical x mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				boolean as y,
				number as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics considers binned x and numerical y mapping valid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(day) as x,
				number as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics considers binned y and numerical x mapping valid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(day) as y,
				number as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical column and",
    "count for positional aesthetics valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				count(*) as y
			from synth
			group by
				letter
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers binning and counting",
    "for positional aesthetics valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as x,
				count(*) as y
			from synth
			group by
				bin(number)
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical x",
    "and temporal y mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				day as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers temporal x and",
    "categorical y mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				day_and_time as x,
				boolean as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics considers binned x and temporal y mapping valid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as x,
				day as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics considers binned y and temporal x mapping valid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as y,
				day as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics considers categorical x and categorical y invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				boolean as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'x' or 'y' should be a numerical or",
      "temporal mapping (unbinned) for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers categorical x and binned y invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as x,
				bin(number) as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'x' or 'y' should be a numerical or",
      "temporal mapping (unbinned) for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that("valid_aesthetics considers binned x and binned y invalid for bars", {
  rgs <- sgl_to_rgs("
    visualize
      bin(number) as x,
      bin(day) as y
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: one of 'x' or 'y' should be a numerical or",
    "temporal mapping (unbinned) for the bar geom"
  )
  expect_error(
    valid_aesthetics(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that(
  "valid_aesthetics considers two non-categorical columns invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				number as x,
				day as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'x' or 'y' should be a categorical",
      "or binned mapping for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers numerical column and count invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				number as x,
				count(*) as y
			from synth
			group by
				number
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'x' or 'y' should be a categorical",
      "or binned mapping for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers temporal column and count invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				count(*) as y
			from synth
			group by
				day
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'x' or 'y' should be a categorical",
      "or binned mapping for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only x is present and x is categorical",
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only y is present and y is categorical",
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only x is present and x is binned",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only y is present and y is binned",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  paste(
    "valid_aesthetics doesn't raise error if",
    "only x is present and x is numerical"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				number as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics doesn't raise error if",
    "only y is present and y is numerical"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				number as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics doesn't raise error if only x is present and x is temporal",
  {
    rgs <- sgl_to_rgs("
			visualize
				day_and_time as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics doesn't raise error if only y is present and y is temporal",
  {
    rgs <- sgl_to_rgs("
			visualize
				day_and_time as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics doesn't raise error if only x is present and is a count",
  {
    rgs <- sgl_to_rgs("
			visualize
				count(*) as x
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics doesn't raise error if only y is present and y is a count",
  {
    rgs <- sgl_to_rgs("
			visualize
				count(*) as y
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical theta",
    "and numerical r mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as theta,
				number as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical r and",
    "numerical theta mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				boolean as r,
				number as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers binned theta",
    "and numerical r mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(day) as theta,
				number as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers binned r and numerical",
    "theta mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(day) as r,
				number as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical column and count",
    "for positional aesthetics valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as theta,
				count(*) as r
			from synth
			group by
				letter
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers binning and counting",
    "for positional aesthetics valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
    visualize
      bin(number) as theta,
      count(*) as r
    from synth
		group by
			bin(number)
    using bars
  ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical theta",
    "and temporal r mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as theta,
				day as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers temporal theta",
    "and categorical r mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				day_and_time as theta,
				boolean as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers binned theta",
    "and temporal r mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as theta,
				day as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers binned r and",
    "temporal theta mapping valid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as r,
				day as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics considers categorical",
    "theta and categorical r invalid for bars"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as theta,
				boolean as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'theta' or 'r' should be a numerical",
      "or temporal mapping (unbinned) for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers categorical theta and binned r invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as theta,
				bin(number) as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'theta' or 'r' should be a numerical",
      "or temporal mapping (unbinned) for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers binned theta and binned r invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as theta,
				bin(day) as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'theta' or 'r' should be a numerical",
      "or temporal mapping (unbinned) for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers two non-categorical columns invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				number as theta,
				day as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'theta' or 'r' should be a categorical",
      "or binned mapping for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers numerical column and count invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				number as theta,
				count(*) as r
			from synth
			group by
				number
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'theta' or 'r' should be a categorical",
      "or binned mapping for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics considers temporal column and count invalid for bars",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as theta,
				count(*) as r
			from synth
			group by
				day
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: one of 'theta' or 'r' should be a",
      "categorical or binned mapping for the bar geom"
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  paste(
    "valid_aesthetics raises error if only theta",
    "is present and theta is categorical"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only r is present and r is categorical",
  {
    rgs <- sgl_to_rgs("
			visualize
				letter as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only theta is present and theta is binned",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "valid_aesthetics raises error if only r is present and r is binned",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(number) as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if only one positional aesthetic is provided for the bar geom,",
      "then it must be mapped to a numerical or temporal type (unbinned)."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  paste(
    "valid_aesthetics doesn't raise error if",
    "only theta is present and theta is numerical"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				number as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics doesn't raise error if",
    "only r is present and r is numerical"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				number as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics doesn't raise error if",
    "only theta is present and theta is temporal"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				day_and_time as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics doesn't raise error if only r is present and r is temporal",
  {
    rgs <- sgl_to_rgs("
			visualize
				day_and_time as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_aesthetics doesn't raise error if",
    "only theta is present and is a count"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				count(*) as theta
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_aesthetics doesn't raise error if only r is present and is a count",
  {
    rgs <- sgl_to_rgs("
			visualize
				count(*) as r
			from synth
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that("valid_aesthetics raises error for mixed coordinate system", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as theta
    from cars
		group by
			bin(mpg)
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: found aesthetics from multiple coordinate systems.",
    "All positional aesthetics must be from a single coordinate system."
  )
  expect_error(
    valid_aesthetics(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that(
  "valid_aesthetics raises error when no positional aesthetics provided",
  {
    rgs <- sgl_to_rgs("
			visualize
				mpg as color
			from cars
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      "Error: positional aesthetics must be provided, none were found.",
      fixed = TRUE
    )
  }
)

test_that("valid_aesthetics doesn't raise error for color aesthetic for bars", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y,
      boolean as color
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_aesthetics(layer$geom_expr$geom, layer, df),
  )
})

test_that(
  paste(
    "valid_aesthetics doesn't raise error for",
    "color with other coordinate systems"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(hp) as theta,
				mpg as r,
				cyl as color
			from cars
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that("valid_aesthetics raises error for size aesthetic for bars", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y,
      boolean as size
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_aesthetics(layer$geom_expr$geom, layer, df),
    "Error: size is not a valid aesthetic for the bar geom.",
    fixed = TRUE
  )
})

test_that(
  "valid_aesthetics raises error for size with other coordinate systems",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(hp) as theta,
				mpg as r,
				cyl as size
			from cars
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      "Error: size is not a valid aesthetic for the bar geom.",
      fixed = TRUE
    )
  }
)

test_that("valid_collections raises error if collect by clause specified", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y
    from cars
    collect by
      vs
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: the collect by clause cannot be",
    "specified for non-collective geom bar."
  )
  expect_error(
    valid_collections(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that("ggplot_aes returns correct aes", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y,
      boolean as color
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "fill"))
  expect_equal(ggplot2::as_label(actual_aes$x), "letter")
  expect_equal(ggplot2::as_label(actual_aes$y), "number")
  expect_equal(ggplot2::as_label(actual_aes$fill), "boolean")
})

test_that("ggplot_aes maps to cta generated columns correctly", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y,
      bin(hp) as color
    from cars
    group by
      bin(mpg),
      bin(hp)
    using bars
    scale by
      log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "fill"))
  expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.log.bin.mpg")
  expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
  expect_equal(ggplot2::as_label(actual_aes$fill), "rsgl.linear.bin.hp")
})

test_that(
  paste(
    "ggplot_aes maps to mix of cta generated",
    "and non cta generated columns correctly"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(mpg) as x,
				count(*) as y,
				vs_cat as color
			from (
				select
					*,
					cast(vs as varchar) as vs_cat
				from cars
			)
			group by
				bin(mpg),
				vs_cat
			using bars
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "fill"))
    expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.linear.bin.mpg")
    expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
    expect_equal(ggplot2::as_label(actual_aes$fill), "vs_cat")
  }
)

test_that("ggplot_aes replaces theta and r with x and y", {
  rgs <- sgl_to_rgs("
    visualize
      letter as theta,
      number as r
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "letter")
  expect_equal(ggplot2::as_label(actual_aes$y), "number")
})

test_that("ggplot_aes adds blank mapping for omitted y aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      number as x
    from synth
    using bar
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "number")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted x aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      number as y
    from synth
    using bar
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "number")
})

test_that("ggplot_aes adds blank mapping for omitted r aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      number as theta
    from synth
    using bar
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "number")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted theta aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      number as r
    from synth
    using bar
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "number")
})

test_that("ggplot_aes has no group aes", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_false("group" %in% names(actual_aes))
})

test_that("valid_qualifier doesn't raise error for default qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
  )
})

test_that("valid_qualifier raises error for the jittered qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using jittered bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    "Error: the jittered qualifier is not supported for the bar geom",
    fixed = TRUE
  )
})

test_that("valid_qualifier raises error for the regression qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using regression bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    "Error: the regression qualifier is not supported for the bar geom",
    fixed = TRUE
  )
})

test_that("valid_qualifier doesn't raise error for the unstacked qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using unstacked bars
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
  )
})
