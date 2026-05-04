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
      bin(hp, 5) as color
    from cars
    group by
      bin(mpg),
      bin(hp, 5)
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
  expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.log.bin.30.mpg")
  expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
  expect_equal(ggplot2::as_label(actual_aes$fill), "rsgl.linear.bin.5.hp")
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
    expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.linear.bin.30.mpg")
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

describe("has_direction", {
  it("returns TRUE", {
    bar <- new_sgl_geom_bar()
    expect_true(
      has_direction(bar)
    )
  })
})

describe("ggplot_dir_from_qual", {
  it("returns x for vertical qual", {
    bar <- new_sgl_geom_bar()
    expect_equal(
      ggplot_dir_from_qual(bar, "vertical"),
      "x"
    )
  })
  it("returns y for horizontal qual", {
    bar <- new_sgl_geom_bar()
    expect_equal(
      ggplot_dir_from_qual(bar, "horizontal"),
      "y"
    )
  })
})
