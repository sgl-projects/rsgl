test_that(
  paste(
    "new_sgl_geom_line returns an instance",
    "of the sgl_geom_line subclass"
  ),
  {
    line <- new_sgl_geom_line()
    expect_equal(attr(line, "class"), c("sgl_geom_line", "sgl_geom"))
  }
)

test_that("new_sgl_geom_line only adds class attribute", {
  line <- new_sgl_geom_line()
  all_attributes <- names(attributes(line))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom_line sets empty list as base object", {
  base_object <- unclass(new_sgl_geom_line())

  expect_equal(base_object, list())
})

test_that("geom_name returns line", {
  line <- new_sgl_geom_line()
  actual <- geom_name(line)
  expect_equal(actual, "line")
})

test_that("is_collective returns TRUE", {
  line <- new_sgl_geom_line()
  expect_equal(is_collective(line), TRUE)
})

test_that("ggplot_geom returns geom_line function", {
  line <- new_sgl_geom_line()
  actual <- ggplot_geom(line)
  expect_equal(actual, ggplot2::geom_line)
})

valid_aesthetics_tests(new_sgl_geom_line(), "color")

test_that(
  "valid_collections doesn't raise error if collect by clause omitted",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using lines
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
    "valid_collections doesn't raise error",
    "for valid collections without grouping"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			collect by
				cyl,
				vs
			using lines
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
    "valid_collections raises error if collections",
    "has transformed column and no groupings"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			collect by
				bin(cyl)
			using lines
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: if no groupings are present then all collection",
      "expressions must be untransformed columns."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  paste(
    "valid_collections doesn't raise error if",
    "collects by non-positional groupings"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(mpg) as x,
				count(*) as y
			from cars
			group by
				bin(mpg),
				cyl
			collect by
				cyl
			using lines
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_collections(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that("valid_collections raises error if collects on positional grouping", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y
    from cars
    group by
      bin(mpg),
      cyl
    collect by
      bin(mpg),
      cyl
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_collections(layer$geom_expr$geom, layer, df),
    "Error: cannot collect on positional groupings.",
    fixed = TRUE
  )
})

test_that(
  "valid_collections raises error if non-positional grouping not collected on",
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(mpg) as x,
				count(*) as y
			from cars
			group by
				bin(mpg),
				cyl,
				vs
			collect by
				cyl
			using lines
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: all non-positional groupings must be included in collect",
      "by clause. 'vs' found in grouping but not in collect by clause."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  paste(
    "valid_collections raises error if collection",
    "does not have corresponding grouping"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				bin(mpg) as x,
				count(*) as y
			from cars
			group by
				bin(mpg)
			collect by
				cyl
			using lines
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: when grouping is present, cannot collect by column",
      "that doesn't have corresponding grouping, found 'cyl'."
    )
    expect_error(
      valid_collections(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)

test_that(
  "ggplot_aes returns correct aes with grouping for x and y aesthetics",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				letter as y
			from synth
			using line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "group"))
    expect_equal(ggplot2::as_label(actual_aes$x), "day")
    expect_equal(ggplot2::as_label(actual_aes$y), "letter")
    expect_equal(actual_aes$group, "1")
  }
)

test_that(
  "ggplot_aes returns correct aes with grouping for numerical color aesthetic",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				letter as y,
				number as color
			from synth
			using line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
    expect_equal(ggplot2::as_label(actual_aes$colour), "number")
    expect_equal(actual_aes$group, "1")
  }
)

test_that(
  "ggplot_aes returns correct aes with grouping for temporal color aesthetic",
  {
    rgs <- sgl_to_rgs("
			visualize
				number as x,
				letter as y,
				day as color
			from synth
			using line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
    expect_equal(ggplot2::as_label(actual_aes$colour), "day")
    expect_equal(actual_aes$group, "1")
  }
)

test_that("ggplot_aes groups by color column for categorical color aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      letter as y,
      boolean as color
    from synth
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
  expect_equal(ggplot2::as_label(actual_aes$colour), "boolean")
  expect_equal(ggplot2::as_label(actual_aes$group), "boolean")
})

test_that("ggplot_aes maps to cta generated columns correctly", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y
    from cars
		group by
			bin(mpg)
    using line
		scale by
			log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.log.bin.mpg")
  expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
  expect_equal(actual_aes$group, "1")
})

test_that("ggplot_aes groups by binned color mapping", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y,
			bin(hp) as color
    from cars
		group by
			bin(mpg),
			bin(hp)
    using line
		scale by
			log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
  expect_equal(ggplot2::as_label(actual_aes$colour), "rsgl.linear.bin.hp")
  expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.linear.bin.hp")
})

test_that("ggplot_aes replaces theta and r with x and y", {
  rgs <- sgl_to_rgs("
    visualize
      hp as theta,
      mpg as r
    from cars
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "hp")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes adds blank mapping for omitted y aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as x
    from cars
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "mpg")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted x aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as y
    from cars
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes adds blank mapping for omitted r aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as theta
    from cars
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "mpg")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted theta aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as r
    from cars
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes includes collection in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    collect by
      vs
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "vs")
})

test_that("ggplot_aes includes unmapped transformed collection in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y
    from cars
    group by
      bin(mpg),
      bin(hp)
    collect by
      bin(hp)
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.linear.bin.hp")
})

test_that("ggplot_aes includes mapped transformed collection in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y,
      bin(hp) as color
    from cars
    group by
      bin(mpg),
      bin(hp)
    collect by
      bin(hp)
    using lines
    scale by
      log(color)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.log.bin.hp")
})

test_that("ggplot_aes includes multiple collections in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    collect by
      vs,
      am
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "interaction(am, vs)")
})

test_that(
  paste(
    "ggplot_aes includes both scales for mapped",
    "transformed collection in group aes"
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
			using lines
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

test_that("valid_qualifier doesn't raise error for default qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      bin(hp) as x,
      count(*) as y,
			count(*) as color
    from cars
		group by
			bin(hp)
    using line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_no_error(
    valid_qualifier(layer$geom_expr$geom, layer, df)
  )
})

test_that("valid_qualifier raises error for jittered qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using jittered line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    "Error: the jittered qualifier is not supported for the line geom.",
    fixed = TRUE
  )
})

test_that("valid_qualifier raises error for unstacked qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using unstacked line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    "Error: the unstacked qualifier is not supported for the line geom.",
    fixed = TRUE
  )
})

test_that(
  paste(
    "valid_qualifier doesn't raise error for regression",
    "when no color or transformation is present"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using regression line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  "valid_qualifier doesn't raise error for regression with categorical color",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				number as y,
				boolean as color
			from synth
			using regression line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that("valid_qualifier raises error for regression with numerical color", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y,
			cyl as color
    from cars
    using regression line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: numerical and temporal color mappings are not",
    "allowed with the regression qualifier"
  )
  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that("valid_qualifier raises error for regression with temporal color", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
      number as y,
			day_and_time as color
    from synth
    using regression line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: numerical and temporal color mappings are",
    "not allowed with the regression qualifier"
  )
  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that("valid_qualifier raises error for regression with transformation", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y
    from cars
		group by
			bin(mpg)
    using regression line
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: column-level transformations and aggregations",
    "are not allowed with the regression qualifier"
  )
  expect_error(
    valid_qualifier(layer$geom_expr$geom, layer, df),
    expected_msg,
    fixed = TRUE
  )
})

test_that(
  paste(
    "valid_qualifier doesn't raise error for valid",
    "qualifier in non-cartesian coordinate system"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as theta,
				mpg as r
			from cars
			using regression line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer$geom_expr$geom, layer, df)
    )
  }
)

test_that(
  paste(
    "valid_qualifier raises error for invalid",
    "qualifier in non-cartesian coordinate system"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as theta,
				mpg as r,
				cyl as color
			from cars
			using regression line
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: numerical and temporal color mappings are not",
      "allowed with the regression qualifier"
    )
    expect_error(
      valid_qualifier(layer$geom_expr$geom, layer, df),
      expected_msg,
      fixed = TRUE
    )
  }
)
