test_that("ggplot has correct data", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(p$data, ggplot2::waiver())
  expect_equal(p$layers[[1]]$data, dfs[[1]])
})

test_that("ggplot has correct aesthetic mapping", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(p$mapping, ggplot2::aes())
  layer_mapping <- p$layers[[1]]$mapping
  expect_equal(names(layer_mapping), c("x", "y"))
  expect_equal(ggplot2::as_label(layer_mapping$x), "hp")
  expect_equal(ggplot2::as_label(layer_mapping$y), "mpg")
})

test_that("ggplot has correct mapping for cta-generated columns", {
  rgs <- sgl_to_rgs("
    visualize
      bin(hp) as x,
			bin(mpg) as y,
			count(*) as color
    from cars
		group by
			bin(hp),
			bin(mpg)
    using points
		scale by
			log(y)
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(p$mapping, ggplot2::aes())
  layer_mapping <- p$layers[[1]]$mapping
  expect_equal(names(layer_mapping), c("x", "y", "colour"))
  expect_equal(ggplot2::as_label(layer_mapping$x), "rsgl.linear.bin.hp")
  expect_equal(ggplot2::as_label(layer_mapping$y), "rsgl.log.bin.mpg")
  expect_equal(ggplot2::as_label(layer_mapping$colour), "rsgl.count")
})

test_that("ggplot has correct group aesthetic mapping with collect by clause", {
  rgs <- sgl_to_rgs("
    visualize
			hp as x,
			mpg as y
    from cars
		collect by
			cyl
    using lines
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  layer_mapping <- p$layers[[1]]$mapping
  expect_true("group" %in% names(layer_mapping))
  expect_equal(ggplot2::as_label(layer_mapping$group), "cyl")
})

test_that("ggplot has mapping to blank and no labs for omitted y aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as x
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(p$mapping, ggplot2::aes())
  layer_mapping <- p$layers[[1]]$mapping
  expect_equal(names(layer_mapping), c("x", "y"))
  expect_equal(ggplot2::as_label(layer_mapping$x), "mpg")
  expect_equal(layer_mapping$y, "")
  labels <- p$labels
  expect_equal(labels$x, "mpg")
  expect_equal(labels$y, NULL)
  theme <- p$theme
  expect_s3_class(theme$axis.ticks.y, "element_blank")
})

test_that("ggplot has mapping to blank and no labs for omitted x aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(p$mapping, ggplot2::aes())
  layer_mapping <- p$layers[[1]]$mapping
  expect_equal(names(layer_mapping), c("x", "y"))
  expect_equal(layer_mapping$x, "")
  expect_equal(ggplot2::as_label(layer_mapping$y), "mpg")
  labels <- p$labels
  expect_equal(labels$x, NULL)
  expect_equal(labels$y, "mpg")
  theme <- p$theme
  expect_s3_class(theme$axis.ticks.x, "element_blank")
})

test_that("ggplot has correct geom for points", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
})

test_that("ggplot has correct geom for bars", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$geom, "GeomBar")
})

test_that("ggplot has correct geom for line", {
  rgs <- sgl_to_rgs("
    visualize
      date as x,
			pop as y
    from economics
    using line
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$geom, "GeomLine")
})

test_that("ggplot has correct geom for box", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
			price as y
    from diamonds
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")
})

test_that("bar geoms are stacked by default", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y,
			boolean as color
    from synth
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$position, "PositionStack")
})

test_that("ggplot has identity position for bars with unstacked qualifier", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y,
			boolean as color
    from synth
    using unstacked bars
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$position, "PositionIdentity")
})

test_that(
  "ggplot has identity stat when there are no transformations or qualifiers",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    expect_s3_class(p$layers[[1]]$stat, "StatIdentity")
  }
)

test_that("ggplot has identity stat when there are cta's but no qualifiers", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
			count(*) as y
    from cars
		group by
			bin(mpg)
    using bars
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$stat, "StatIdentity")
})

test_that(
  paste(
    "ggplot has smooth stat with lm method when",
    "there is a regression qualifier"
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

    p <- rgs_to_ggplot2(rgs, dfs)

    expect_s3_class(p$layers[[1]]$stat, "StatSmooth")
    expect_equal(p$layers[[1]]$stat_params$method, "lm")
    expect_equal(p$layers[[1]]$stat_params$formula, y ~ x,
                 ignore_formula_env = TRUE)
  }
)

test_that(
  paste(
    "ggplot has identity position for points",
    "when there is no qualifier"
  ),
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    expect_s3_class(p$layers[[1]]$position, "PositionIdentity")
  }
)

test_that(
  "ggplot has jitter position when there is a jittered qualifier",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using jittered points
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    expect_s3_class(p$layers[[1]]$position, "PositionJitter")
  }
)

test_that("ggplot has boxplot stat for box geom", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
			price as y
    from diamonds
    using boxes
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_s3_class(p$layers[[1]]$stat, "StatBoxplot")
})

test_that("ggplot doesn't include facet columns in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      cyl as x,
			count(*) as y
    from cars
		group by
			cyl,
			vs
    using points
		facet by
			am
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  layer_mapping <- p$layers[[1]]$mapping
  expect_false("group" %in% names(layer_mapping))
})

test_that("ggplot has default scales when no scale by clause provided", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(p$scales$scales, list())
})

test_that("ggplot has scales from scale by clause", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y,
			cyl as color
    from cars
    using points
		scale by
			log(x),
			linear(color)
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(length(p$scales$scales), 2)
  expect_equal(p$scales$scales[[2]]$aesthetics[1], "x")
  expect_equal(class(p$scales$scales[[2]])[1], "ScaleContinuousPosition")
  expect_equal(p$scales$scales[[2]]$trans$name, "log-10")
  expect_equal(p$scales$scales[[1]]$aesthetics[1], "colour")
  expect_equal(class(p$scales$scales[[1]])[1], "ScaleContinuous")
  expect_equal(p$scales$scales[[1]]$trans$name, "identity")
})

test_that("ggplot has cartesian coordinates for x and y aesthetics", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(class(p$coordinates)[1], "CoordCartesian")
})

test_that("ggplot has polar coordinates for theta and r aesthetics", {
  rgs <- sgl_to_rgs("
    visualize
      hp as theta,
			mpg as r
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(class(p$coordinates)[1], "CoordPolar")
})

test_that("ggplot maps theta to x and r to y in polar coordinates", {
  rgs <- sgl_to_rgs("
    visualize
      hp as theta,
			mpg as r
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  layer_mapping <- p$layers[[1]]$mapping
  expect_equal(names(layer_mapping), c("x", "y"))
  expect_equal(ggplot2::as_label(layer_mapping$x), "hp")
  expect_equal(ggplot2::as_label(layer_mapping$y), "mpg")
  expect_equal(p$coordinates$theta, "x")
  expect_equal(p$coordinates$r, "y")
})

test_that("ggplot has one layer for single layer rgs", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(length(p$layers), 1)
})

test_that(
  "ggplot has multiple layers with correct properties for multiple layer rgs",
  {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y,
				cyl as color
			from cars
			using points

			layer

			visualize
				hp as x,
				mpg as y
			from (
				select *
				from cars
				where cyl = 6
			)
			using regression line

			scale by
				log(x)
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    expect_equal(length(p$layers), 2)
    expect_equal(p$data, ggplot2::waiver())
    expect_equal(p$mapping, ggplot2::aes())

    layer_1 <- p$layers[[1]]
    expect_equal(layer_1$data, dfs[[1]])
    layer_1_mapping <- layer_1$mapping
    expect_equal(names(layer_1_mapping), c("x", "y", "group"))
    expect_equal(ggplot2::as_label(layer_1_mapping$x), "hp")
    expect_equal(ggplot2::as_label(layer_1_mapping$y), "mpg")
    expect_equal(layer_1_mapping$group, "1")
    expect_s3_class(layer_1$geom, "GeomLine")
    expect_s3_class(layer_1$stat, "StatSmooth")
    expect_equal(layer_1$stat_params$method, "lm")

    layer_2 <- p$layers[[2]]
    expect_equal(layer_2$data, dfs[[2]])
    layer_2_mapping <- layer_2$mapping
    expect_equal(names(layer_2_mapping), c("x", "y", "colour"))
    expect_equal(ggplot2::as_label(layer_2_mapping$x), "hp")
    expect_equal(ggplot2::as_label(layer_2_mapping$y), "mpg")
    expect_equal(ggplot2::as_label(layer_2_mapping$colour), "cyl")
    expect_s3_class(layer_2$geom, "GeomPoint")
    expect_s3_class(layer_2$stat, "StatIdentity")

    expect_equal(length(p$scales$scales), 1)
    expect_equal(p$scales$scales[[1]]$aesthetics[1], "x")
    expect_equal(class(p$scales$scales[[1]])[1], "ScaleContinuousPosition")
    expect_equal(p$scales$scales[[1]]$trans$name, "log-10")
  }
)

test_that("ggplot has cartesian coordinates for x and y with multiple layers", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points

		layer

		visualize
			hp as x,
			mpg as y
		from cars
		using regression line
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(class(p$coordinates)[1], "CoordCartesian")
})

test_that("ggplot has polar coordinates for theta and r with multiple layers", {
  rgs <- sgl_to_rgs("
    visualize
      hp as theta,
			mpg as r
    from cars
    using points

		layer

		visualize
			hp as theta,
			mpg as r
		from cars
		using regression line
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(class(p$coordinates)[1], "CoordPolar")
  layer_mapping_1 <- p$layers[[1]]$mapping
  expect_equal(names(layer_mapping_1), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(layer_mapping_1$x), "hp")
  expect_equal(ggplot2::as_label(layer_mapping_1$y), "mpg")
  layer_mapping_2 <- p$layers[[2]]$mapping
  expect_equal(names(layer_mapping_2), c("x", "y"))
  expect_equal(ggplot2::as_label(layer_mapping_2$x), "hp")
  expect_equal(ggplot2::as_label(layer_mapping_2$y), "mpg")
  expect_equal(p$coordinates$theta, "x")
  expect_equal(p$coordinates$r, "y")
})

test_that("ggplot has no faceting when facet by clause omitted", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
			mpg as y
    from cars
    using points
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_equal(class(p$facet)[1], "FacetNull")
})

test_that("ggplot has column facet for single default facet", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y
    from synth
    using points
		facet by
			boolean
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  facet <- p$facet
  expect_equal(class(facet)[1], "FacetGrid")
  expect_equal(names(facet$params$cols), "boolean")
  expect_equal(names(facet$params$rows), character(0))
})

test_that("ggplot has column facet for single horizontal facet", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y
    from synth
    using points
		facet by
			boolean horizontally
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  facet <- p$facet
  expect_equal(class(facet)[1], "FacetGrid")
  expect_equal(names(facet$params$cols), "boolean")
  expect_equal(names(facet$params$rows), character(0))
})

test_that("ggplot has row facet for single vertical facet", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y
    from synth
    using points
		facet by
			boolean vertically
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  facet <- p$facet
  expect_equal(class(facet)[1], "FacetGrid")
  expect_equal(names(facet$params$cols), character(0))
  expect_equal(names(facet$params$rows), "boolean")
})

test_that("ggplot has correct facets for two default facets", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
			number as y
    from synth
    using points
		facet by
			letter,
			boolean
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  facet <- p$facet
  expect_equal(class(facet)[1], "FacetGrid")
  expect_equal(names(facet$params$cols), "letter")
  expect_equal(names(facet$params$rows), "boolean")
})

test_that(
  "ggplot has correct facets for one default and one horizontal facet",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				number as y
			from synth
			using points
			facet by
				letter,
				boolean	horizontally
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    facet <- p$facet
    expect_equal(class(facet)[1], "FacetGrid")
    expect_equal(names(facet$params$cols), "boolean")
    expect_equal(names(facet$params$rows), "letter")
  }
)

test_that("ggplot has correct facets for one default and one vertical facet", {
  rgs <- sgl_to_rgs("
    visualize
      day as x,
			number as y
    from synth
    using points
		facet by
			letter,
			boolean vertically
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  facet <- p$facet
  expect_equal(class(facet)[1], "FacetGrid")
  expect_equal(names(facet$params$cols), "letter")
  expect_equal(names(facet$params$rows), "boolean")
})

test_that(
  "ggplot has correct facets for two explicit facets with horizontal first",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				number as y
			from synth
			using points
			facet by
				letter horizontally,
				boolean vertically
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    facet <- p$facet
    expect_equal(class(facet)[1], "FacetGrid")
    expect_equal(names(facet$params$cols), "letter")
    expect_equal(names(facet$params$rows), "boolean")
  }
)

test_that(
  "ggplot has correct facets for two explicit facets with vertical first",
  {
    rgs <- sgl_to_rgs("
			visualize
				day as x,
				number as y
			from synth
			using points
			facet by
				letter vertically,
				boolean horizontally
		")
    dfs <- result_dfs(rgs, test_con)

    p <- rgs_to_ggplot2(rgs, dfs)

    facet <- p$facet
    expect_equal(class(facet)[1], "FacetGrid")
    expect_equal(names(facet$params$cols), "boolean")
    expect_equal(names(facet$params$rows), "letter")
  }
)

test_that("ggplot has correct facet with multiple layers", {
  rgs <- sgl_to_rgs("
    visualize
      letter as x,
			number as y
    from synth
    using points

		layer

		visualize
			letter as x,
			number_plus_one as y
		from (
			select
				*,
				number + 1 as number_plus_one
			from synth
		)
		using points

		facet by
			boolean
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  facet <- p$facet
  expect_equal(class(facet)[1], "FacetGrid")
  expect_equal(names(facet$params$cols), "boolean")
  expect_equal(names(facet$params$rows), character(0))
})

test_that("ggplot has correct labs", {
  rgs <- sgl_to_rgs("
    visualize
      hp as x,
      mpg as y
    from cars
    using points

    layer

    visualize
      hp as x,
      mpg as y,
      cyl as color
    from cars
    using points

    title
      y as 'Miles Per Gallon'
  ")
  dfs <- result_dfs(rgs, test_con)

  p <- rgs_to_ggplot2(rgs, dfs)

  expect_setequal(names(p$labels), c("x", "y", "colour"))
  expect_equal(p$labels$x, "hp")
  expect_equal(p$labels$y, "Miles Per Gallon")
  expect_equal(p$labels$colour, "cyl")
})
