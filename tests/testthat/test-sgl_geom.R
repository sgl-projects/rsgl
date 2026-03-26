test_that("new_sgl_geom returns an instance of the sgl_geom class", {
  geom <- new_sgl_geom()
  expect_equal(attr(geom, "class"), "sgl_geom")
})

test_that("new_sgl_geom only adds class attribute", {
  geom <- new_sgl_geom()
  all_attributes <- names(attributes(geom))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom sets empty list as base object", {
  base_object <- unclass(new_sgl_geom())

  expect_equal(base_object, list())
})

test_that("new_sgl_geom adds provided subclass", {
  geom <- new_sgl_geom("sgl_geom_xyz")
  expect_equal(attr(geom, "class"), c("sgl_geom_xyz", "sgl_geom"))
})

test_that("new_sgl_geom only adds class attribute for subclasses", {
  geom <- new_sgl_geom("sgl_geom_xyz")
  all_attributes <- names(attributes(geom))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom sets empty list as base object for subclass", {
  base_object <- unclass(new_sgl_geom("sgl_geom_xyz"))

  expect_equal(base_object, list())
})

test_that("geom_name returns geom", {
  geom <- new_sgl_geom()
  actual <- geom_name(geom)
  expect_equal(actual, "geom")
})

test_that("is_collective returns false", {
  geom <- new_sgl_geom()
  expect_equal(is_collective(geom), FALSE)
})

valid_aesthetics_tests(new_sgl_geom(), .non_pos_aes)

valid_qualifier_tests(new_sgl_geom(), .all_quals)

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
  layer$geom_expr$geom <- new_sgl_geom()

  expected_msg <- paste(
    "Error: the collect by clause cannot be",
    "specified for non-collective geom geom."
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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
    layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes has no group aes if collections aren't specified", {
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
  layer$geom_expr$geom <- new_sgl_geom()

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_false("group" %in% names(actual_aes))
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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
  layer$geom_expr$geom <- new_sgl_geom()

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
    layer$geom_expr$geom <- new_sgl_geom()

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(rsgl.log.bin.mpg, rsgl.linear.bin.mpg)"
    )
  }
)
