test_that(
  paste(
    "new_sgl_geom_point returns an instance",
    "of the sgl_geom_point subclass"
  ),
  {
    point <- new_sgl_geom_point()
    expect_equal(attr(point, "class"), c("sgl_geom_point", "sgl_geom"))
  }
)

test_that("new_sgl_geom_point only adds class attribute", {
  point <- new_sgl_geom_point()
  all_attributes <- names(attributes(point))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom_point sets empty list as base object", {
  base_object <- unclass(new_sgl_geom_point())

  expect_equal(base_object, list())
})

test_that("geom_name returns point", {
  point <- new_sgl_geom_point()
  actual <- geom_name(point)
  expect_equal(actual, "point")
})

test_that("is_collective returns false", {
  point <- new_sgl_geom_point()
  expect_equal(is_collective(point), FALSE)
})

test_that("ggplot_geom returns geom_point function", {
  point <- new_sgl_geom_point()
  actual <- ggplot_geom(point)
  expect_equal(actual, ggplot2::geom_point)
})

valid_aesthetics_tests(new_sgl_geom_point(), .non_pos_aes)

valid_qualifier_tests(new_sgl_geom_point(), "jittered")

test_that("valid_collections raises error if collect by clause specified", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    collect by
      vs
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  expected_msg <- paste(
    "Error: the collect by clause cannot be",
    "specified for non-collective geom point."
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
      hp as x,
      mpg as y,
      cyl as color,
      cyl as size
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "size", "colour"))
  expect_equal(ggplot2::as_label(actual_aes$x), "hp")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
  expect_equal(ggplot2::as_label(actual_aes$size), "cyl")
  expect_equal(ggplot2::as_label(actual_aes$colour), "cyl")
})

test_that("ggplot_aes maps to cta generated columns correctly", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      bin(hp) as y,
      count(*) as color
    from cars
    group by
      bin(mpg),
      bin(hp)
    using points
    scale by
      log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour"))
  expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.log.bin.mpg")
  expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.linear.bin.hp")
  expect_equal(ggplot2::as_label(actual_aes$colour), "rsgl.count")
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
			using points
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "colour"))
    expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.linear.bin.mpg")
    expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
    expect_equal(ggplot2::as_label(actual_aes$colour), "vs_cat")
  }
)

test_that("ggplot_aes replaces theta and r with x and y", {
  rgs <- sgl_to_rgs("
    visualize
      hp as theta,
      mpg as r
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "hp")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes adds blank mapping for omitted y aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as x
    from cars
    using points
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
    using points
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
    using points
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
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes has no group aes", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_false("group" %in% names(actual_aes))
})

