test_box <- new_sgl_geom_box()

test_that("new_sgl_geom_box returns an instance of the sgl_geom_box subclass", {
  expect_equal(attr(test_box, "class"), c("sgl_geom_box", "sgl_geom"))
})

test_that("new_sgl_geom_box only adds class attribute", {
  all_attributes <- names(attributes(test_box))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom_box sets empty list as base object", {
  base_object <- unclass(test_box)

  expect_equal(base_object, list())
})

test_that("geom_name returns box", {
  actual <- geom_name(test_box)
  expect_equal(actual, "box")
})

test_that("is_collective returns TRUE", {
  expect_equal(is_collective(test_box), TRUE)
})

test_that("ggplot_geom returns geom_boxplot function", {
  actual <- ggplot_geom(test_box)
  expect_equal(actual, ggplot2::geom_boxplot)
})

test_that("valid_aesthetics raises error for mixed coordinate system", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as theta
    from synth
    using boxes
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
				letter as color
			from synth
			using boxes
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

valid_two_pos_aes_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr_1, ~expr_2,
    "cat & num", "letter", "number",
    "cat & tmp", "letter", "day",
    "cat & cnt", "letter", "count(*)",
    "bin num & num", "bin(number)", "number",
    "bin num & tmp", "bin(number)", "day",
    "bin num & cnt", "bin(number)", "count(*)",
    "bin tmp & num", "bin(day)", "number",
    "bin tmp & tmp", "bin(day)", "day",
    "bin tmp & cnt", "bin(day)", "count(*)"
  )
}
valid_two_pos_aes_body <- function(aes_1, aes_2, expr_1, expr_2) {
  sgl <- sprintf(
    "
			visualize
				%s as %s,
				%s as %s
			from synth
			using boxes
		",
    expr_1,
    aes_1,
    expr_2,
    aes_2
  )
  rgs <- sgl_to_rgs(sgl)
  dfs <- result_dfs(rgs, test_con) # nolint: object_usage_linter
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error( # nolint: object_usage_linter
    valid_aesthetics(layer$geom_expr$geom, layer, df)
  )
}
patrick::with_parameters_test_that(
  "considers two positional aesthetics valid (x & y):",
  {
    valid_two_pos_aes_body("x", "y", expr_1, expr_2)
  },
  .cases = valid_two_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers two positional aesthetics valid (y & x):",
  {
    valid_two_pos_aes_body("x", "y", expr_1, expr_2)
  },
  .cases = valid_two_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers two positional aesthetics valid (theta & r):",
  {
    valid_two_pos_aes_body("theta", "r", expr_1, expr_2)
  },
  .cases = valid_two_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers two positional aesthetics valid (r & theta):",
  {
    valid_two_pos_aes_body("r", "theta", expr_1, expr_2)
  },
  .cases = valid_two_pos_aes_cases()
)

invalid_two_pos_aes_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr_2, ~expr_1,
    "cat & cat", "letter", "boolean",
    "cat & bin num", "letter", "bin(number)",
    "cat & bin tmp", "letter", "bin(day)",
    "num & num", "number", "number",
    "num & tmp", "number", "day",
    "num & cnt", "number", "count(*)",
    "tmp & tmp", "day", "day_and_time",
    "tmp & cnt", "day", "count(*)",
    "bin num & bin num", "bin(number)", "bin(number)",
    "bin num & bin tmp", "bin(number)", "bin(day)",
    "bin tmp & bin tmp", "bin(day)", "bin(day_and_time)",
    "cnt & cnt", "count(*)", "count(*)"
  )
}
invalid_two_pos_aes_body <- function(aes_1, aes_2, expr_1, expr_2) {
  sgl <- sprintf(
    "
			visualize
				%s as %s,
				%s as %s
			from synth
			using boxes
		",
    expr_1,
    aes_1,
    expr_2,
    aes_2
  )
  rgs <- sgl_to_rgs(sgl)
  dfs <- result_dfs(rgs, test_con) # nolint: object_usage_linter
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: for a box geom with two positional aesthetic mappings,",
    "one mapping must be categorical or binned,",
    "and the other must be numerical or temporal (unbinned)."
  )
  expect_error( # nolint: object_usage_linter
    valid_aesthetics(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
}
patrick::with_parameters_test_that(
  "considers two positional aesthetics invalid (x & y):",
  {
    invalid_two_pos_aes_body("x", "y", expr_1, expr_2)
  },
  .cases = invalid_two_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers two positional aesthetics invalid (y & x):",
  {
    invalid_two_pos_aes_body("y", "x", expr_1, expr_2)
  },
  .cases = invalid_two_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers two positional aesthetics invalid (theta & r):",
  {
    invalid_two_pos_aes_body("theta", "r", expr_1, expr_2)
  },
  .cases = invalid_two_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers two positional aesthetics invalid (r & theta):",
  {
    invalid_two_pos_aes_body("r", "theta", expr_1, expr_2)
  },
  .cases = invalid_two_pos_aes_cases()
)

valid_one_pos_aes_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "num", "number",
    "tmp", "day",
    "cnt", "count(*)"
  )
}
valid_one_pos_aes_body <- function(aes, expr) {
  sgl <- sprintf(
    "
			visualize
				%s as %s
			from synth
			using box
		",
    expr,
    aes
  )
  rgs <- sgl_to_rgs(sgl)
  dfs <- result_dfs(rgs, test_con) # nolint: object_usage_linter
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error( # nolint: object_usage_linter
    valid_aesthetics(layer$geom_expr$geom, layer, df)
  )
}
patrick::with_parameters_test_that(
  "considers one positional aesthetic valid (x):",
  {
    valid_one_pos_aes_body("x", expr)
  },
  .cases = valid_one_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers one positional aesthetic valid (y):",
  {
    valid_one_pos_aes_body("y", expr)
  },
  .cases = valid_one_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers one positional aesthetic valid (theta):",
  {
    valid_one_pos_aes_body("theta", expr)
  },
  .cases = valid_one_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers one positional aesthetic valid (r):",
  {
    valid_one_pos_aes_body("r", expr)
  },
  .cases = valid_one_pos_aes_cases()
)

invalid_one_pos_aes_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "cat", "letter",
    "bin num", "bin(number)",
    "bin tmp", "bin(day)"
  )
}
invalid_one_pos_aes_body <- function(aes, expr) {
  sgl <- sprintf(
    "
			visualize
				%s as %s
			from synth
			using box
		",
    expr,
    aes
  )
  rgs <- sgl_to_rgs(sgl)
  dfs <- result_dfs(rgs, test_con) # nolint: object_usage_linter
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: for a box geom with one positional aesthetic mapping,",
    "the mapping must be to a numerical or temporal (unbinned) type."
  )
  expect_error( # nolint: object_usage_linter
    valid_aesthetics(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
}
patrick::with_parameters_test_that(
  "considers one positional aesthetic invalid (x):",
  {
    invalid_one_pos_aes_body("x", expr)
  },
  .cases = invalid_one_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers one positional aesthetic invalid (y):",
  {
    invalid_one_pos_aes_body("y", expr)
  },
  .cases = invalid_one_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers one positional aesthetic invalid (theta):",
  {
    invalid_one_pos_aes_body("theta", expr)
  },
  .cases = invalid_one_pos_aes_cases()
)
patrick::with_parameters_test_that(
  "considers one positional aesthetic invalid (r):",
  {
    invalid_one_pos_aes_body("r", expr)
  },
  .cases = invalid_one_pos_aes_cases()
)

test_that("valid_aesthetics raises error for size mapping", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y,
			boolean as size
    from synth
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_aesthetics(layer$geom_expr$geom, layer, df),
    "Error: the size aesthetic is not valid for the box geom.",
    fixed = TRUE
  )
})

valid_color_mapping_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "cat", "boolean",
    "bin num", "bin(number)",
    "bin tmp", "bin(day)"
  )
}
patrick::with_parameters_test_that(
  "valid_aesthetics doesn't raise error for valid color mapping:",
  {
    sgl <- sprintf(
      "
				visualize
					letter as x,
					number as y,
					%s as color
				from synth
				using boxes
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df)
    )
  },
  .cases = valid_color_mapping_cases()
)

invalid_color_mapping_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "num", "number",
    "tmp", "day",
    "cnt", "count(*)" # no group by in sgl stmt but that doesn't impact test
  )
}
patrick::with_parameters_test_that(
  "valid_aesthetics raises error for invalid color mapping:",
  {
    sgl <- sprintf(
      "
				visualize
					letter as x,
					number as y,
					%s as color
				from synth
				using boxes
			",
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: for the box geom, color can only be",
      "mapped to a categorical or binned column."
    )
    expect_error(
      valid_aesthetics(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  },
  .cases = invalid_color_mapping_cases()
)

test_that("valid_collections doesn't raise error for default collection", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_collections(layer$geom_expr$geom, layer, df),
  )
})

test_that("valid_collections raises error for count", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
		collect by
			cut,
			count(*)
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_collections(layer$geom_expr$geom, layer, df),
    "Error: cannot collect by a count.",
    fixed = TRUE
  )
})

test_that("valid_collections doesn't raise error for bin", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
		collect by
			cut,
			bin(carat)
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_collections(layer$geom_expr$geom, layer, df)
  )
})

cat_bin_pos_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "cat", "cut",
    "bin", "bin(carat)"
  )
}
patrick::with_parameters_test_that(
  "valid_collections doesn't raise error for positional collection:",
  {
    sgl <- sprintf(
      "
				visualize
					%s as x,
					price as y
				from diamonds
				collect by
					%s
				using boxes
      ",
      expr,
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_collections(layer$geom_expr$geom, layer, df)
    )
  },
  .cases = cat_bin_pos_cases()
)

patrick::with_parameters_test_that(
  "valid_collections raises error if positional cat/binned aes not included:",
  {
    sgl <- sprintf(
      "
				visualize
					%s as x,
					price as y
				from diamonds
				collect by
					clarity
				using boxes
      ",
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: For the box geom, categorical or binned",
      "columns mapped to positional aesthetics must",
      "be included in an explicit collection if it is provided."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  },
  .cases = cat_bin_pos_cases()
)

num_tmp_pos_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr,
    "num", "number",
    "tmp", "day"
  )
}
patrick::with_parameters_test_that(
  paste(
    "valid_collections raises error if positional non-categorical,",
    "non-binned aes is included (2 pos aes):"
  ),
  {
    sgl <- sprintf(
      "
				visualize
					letter as x,
					%s as y
				from synth
				collect by
					letter,
					%s
				using boxes
      ",
      expr,
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: For the box geom, unbinned numerical or temporal columns mapped",
      "to positional aesthetics cannot be included in an explicit collection."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  },
  .cases = num_tmp_pos_cases()
)
patrick::with_parameters_test_that(
  paste(
    "valid_collections raises error if positional non-categorical,",
    "non-binned aes is included (1 pos aes):"
  ),
  {
    sgl <- sprintf(
      "
				visualize
					%s as x
				from synth
				collect by
					%s
				using boxes
      ",
      expr,
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: For the box geom, unbinned numerical or temporal columns mapped",
      "to positional aesthetics cannot be included in an explicit collection."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  },
  .cases = num_tmp_pos_cases()
)

test_that(
  "valid_collections doesn't raise error if color mapping is included",
  {
    rgs <- sgl_to_rgs("
			visualize
				cut as x,
				price as y,
				clarity as color
			from diamonds
			collect by
				cut,
				clarity
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_collections(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that("valid_collections raises error if color mapping is not included", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y,
			clarity as color
    from diamonds
		collect by
			cut
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: For the box geom, color mappings must have corresponding",
    "collections if an explicit collection is provided."
  )
  expect_error(
    valid_collections(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that(
  paste(
    "valid_collections doesn't raise error for",
    "collections without corresponding aesthetic mapping"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				cut as x,
				price as y
			from diamonds
			collect by
				cut,
				clarity
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_collections(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_collections doesn't raise error for",
    "collections with corresponding grouping"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				cut as x,
				count(*) as y
			from diamonds
			group by
				clarity,
				cut,
				color
			collect by
				cut,
				clarity
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_collections(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_collections raises error for collection",
    "without corresponding grouping"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				cut as x,
				count(*) as y
			from diamonds
			group by
				cut,
				clarity
			collect by
				cut,
				color
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: For the box geom with a group by clause, cannot collect on",
      "an expression that doesn't have a corresponding grouping."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that("ggplot_aes returns correct aes mappings", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y,
      clarity as color
    from diamonds
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "cut")
  expect_equal(ggplot2::as_label(actual_aes$y), "price")
  expect_equal(ggplot2::as_label(actual_aes$colour), "clarity")
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
    using boxes
    scale by
      log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.log.bin.mpg")
  expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
  expect_equal(ggplot2::as_label(actual_aes$colour), "rsgl.linear.bin.hp")
})

test_that(
  paste(
    "ggplot_aes maps to mix of cta generated and",
    "non cta generated columns correctly"
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
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
    expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.linear.bin.mpg")
    expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
    expect_equal(ggplot2::as_label(actual_aes$colour), "vs_cat")
  }
)

test_that("ggplot_aes replaces theta and r with x and y", {
  rgs <- sgl_to_rgs("
    visualize
      cut as theta,
      price as r
    from diamonds
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "cut")
  expect_equal(ggplot2::as_label(actual_aes$y), "price")
})

test_that("ggplot_aes adds blank mapping for omitted y aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as x
    from cars
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "mpg")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted x aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as y
    from cars
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes adds blank mapping for omitted r aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as theta
    from cars
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "mpg")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted theta aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as r
    from cars
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes includes explicit collection in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
    collect by
      cut
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "cut")
})

test_that("ggplot_aes includes multiple explicit collections in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
    collect by
      cut,
      clarity
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "interaction(clarity, cut)")
})

test_that(
  "ggplot_aes includes unmapped transformed explicit collection in group aes",
  {
    rgs <- sgl_to_rgs("
			visualize
				cut as x,
				price as y
			from diamonds
			collect by
				cut,
				bin(carat)
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(rsgl.linear.bin.carat, cut)"
    )
  }
)

test_that(
  "ggplot_aes includes mapped transformed explicit collection in group aes",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(carat) as x,
				price as y
			from diamonds
			collect by
				bin(carat)
			using boxes
			scale by
				log(x)
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.log.bin.carat")
  }
)

test_that(
  paste(
    "ggplot_aes includes both scales for mapped",
    "transformed explicit collection in group aes"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(mpg) as x,
				count(*) as y,
				bin(mpg) as color
			from cars
			group by
				bin(mpg)
			collect by
				bin(mpg)
			using boxes
			scale by
				log(color)
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(rsgl.log.bin.mpg, rsgl.linear.bin.mpg)"
    )
  }
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_aes has no group aes by default for single",
    "pos aes with no additional aesthetics:"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				mpg as x
			from cars
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_false("group" %in% names(actual_aes))
  },
  aes = .pos_aes,
  .test_name = aes
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_aes has only cat pos mapping in group aes by default",
    "for two pos aes with no additional aesthetics:"
  ),
  {
    if (aes %in% .cart_aes) {
      other_aes <- setdiff(.cart_aes, aes)
    } else {
      other_aes <- setdiff(.polar_aes, aes)
    }
    sgl <- sprintf(
      "
				visualize
					cut as %s,
					price as %s
				from diamonds
				using boxes
			",
      aes,
      other_aes
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), "cut")
  },
  aes = .pos_aes,
  .test_name = paste("cat", aes)
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_aes has only binned pos mapping in group aes by default",
    "for two pos aes with no additional aesthetics:"
  ),
  {
    if (aes %in% .cart_aes) {
      other_aes <- setdiff(.cart_aes, aes)
    } else {
      other_aes <- setdiff(.polar_aes, aes)
    }
    sgl <- sprintf(
      "
				visualize
					bin(carat) as %s,
					price as %s
				from diamonds
				using boxes
			",
      aes,
      other_aes
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.linear.bin.carat")
  },
  aes = .pos_aes,
  .test_name = paste("binned", aes)
)

patrick::with_parameters_test_that(
  paste(
    "ggplot_aes has only scaled binned pos mapping in group aes",
    "by default for two pos aes with no additional aesthetics:"
  ),
  {
    if (aes %in% .cart_aes) {
      other_aes <- setdiff(.cart_aes, aes)
    } else {
      other_aes <- setdiff(.polar_aes, aes)
    }
    sgl <- sprintf(
      "
				visualize
					bin(carat) as %s,
					price as %s
				from diamonds
				using boxes
				scale by
					log(%s)
			",
      aes,
      other_aes,
      aes
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.log.bin.carat")
  },
  aes = .pos_aes,
  .test_name = paste("binned", aes)
)

color_cases <- function() {
  tibble::tribble(
    ~.test_name, ~mapping_expr, ~scale_expr, ~expected_group_cmpnt,
    "cat color", "clarity", "", "clarity",
    "binned color", "bin(carat)", "", "rsgl.linear.bin.carat",
    "log-bin color", "bin(carat)", "scale by log(color)", "rsgl.log.bin.carat"
  )
}
patrick::with_parameters_test_that(
  "ggplot_aes has color mapping in group aes by default for one pos aes:",
  {
    sgl <- sprintf(
      "
				visualize
					price as x,
					%s as color
				from diamonds
				using boxes
				%s
			",
      mapping_expr,
      scale_expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), expected_group_cmpnt)
  },
  .cases = color_cases()
)

patrick::with_parameters_test_that(
  "ggplot_aes has color mapping in group aes by default for two pos aes:",
  {
    sgl <- sprintf(
      "
				visualize
					cut as x,
					price as y,
					%s as color
				from diamonds
				using boxes
				%s
			",
      mapping_expr,
      scale_expr
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expected_group_mapping <- sprintf(
      "interaction(cut, %s)",
      expected_group_cmpnt
    )
    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), expected_group_mapping)
  },
  .cases = color_cases()
)

test_that(
  paste(
    "ggplot_aes doesn't duplicate default grouping",
    "if pos and color mapping are same"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				cut as x,
				price as y,
				cut as color
			from diamonds
			using boxes
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), "cut")
  }
)

test_that(
  paste(
    "ggplot_aes includes both scales in default",
    "grouping if pos and color are scaled differently"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(carat) as x,
				price as y,
				bin(carat) as color
			from diamonds
			using boxes
			scale by
				log(color)
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(rsgl.linear.bin.carat, rsgl.log.bin.carat)"
    )
  }
)

test_that("valid_qualifier doesn't raise error for default qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
      number as y
    from synth
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
  )
})

invalid_qualifier_cases <- function() {
  tibble::tribble(
    ~.test_name, ~qualifier,
    "jittered", "jittered",
    "regression", "regression",
    "unstacked", "unstacked"
  )
}
patrick::with_parameters_test_that(
  "valid_qualifier raises error for non-default qualifiers:",
  {
    sgl <- sprintf(
      "
				visualize
					letter as x,
					number as y
				from synth
				using %s boxes
			",
      qualifier
    )
    rgs <- sgl_to_rgs(sgl)
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_error(
      valid_qualifier(layer$geom_expr$geom, layer, df),
      "Error: geom qualifiers are not allowed for the box geom.",
      fixed = TRUE
    )
  },
  .cases = invalid_qualifier_cases()
)
