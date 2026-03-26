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

valid_aesthetics_tests(test_box, "color")

valid_qualifier_tests(test_box, character())

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

