test_that("generates scatterplot", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y
		from cars
		using points
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("scatterplot", p)
})

test_that("generates bar chart", {
  sgl_stmt <- "
		visualize
			letter as x,
			number as y,
			boolean as color
		from synth
		using bars
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("bar chart", p)
})

test_that("generates unstacked bar chart", {
  sgl_stmt <- "
		visualize
			letter as x,
			number as y,
			boolean as color
		from synth
		using unstacked bars
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("unstacked bar chart", p)
})

test_that("generates line chart", {
  sgl_stmt <- "
		visualize
			date as x,
			pop as y
		from economics
		using line
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("line chart", p)
})

test_that("line chart default collection on color column", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using lines
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger(
    "line chart default collection on color column",
    p
  )
})

test_that("generates histogram", {
  sgl_stmt <- "
		visualize
			bin(mpg) as x,
			count(*) as y
		from cars
		group by
			bin(mpg)
		using bars
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("histogram", p)
})

test_that("generates histogram across non-positional groupings", {
  sgl_stmt <- "
		visualize
			bin(mpg) as x,
			count(*) as y,
			cyl_cat as color
		from (
			select
				*,
				cast(cyl as varchar) as cyl_cat
			from cars
		)
		group by
			bin(mpg),
			cyl_cat
		using bars
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("histogram across groups", p)
})

test_that("generates horizontal bar histogram without qualifier", {
  sgl_stmt <- "
		visualize
			bin(mpg) as y,
			count(*) as x
		from cars
		group by
			bin(mpg)
		using bars
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("horizontal bar histogram without qualifier", p)
})

test_that("generates line plot with multiple collective lines", {
  sgl_stmt <- "
		visualize
			letter as x,
			number as y
		from synth
		collect by
			boolean
		using lines
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("line plot with multiple collective lines", p)
})

test_that("generates box plot", {
  sgl_stmt <- "
		visualize
			cut as x,
			price as y
		from diamonds
		using boxes
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("boxplot", p)
})

test_that("generates box plot with default collection on numerical mappings", {
  sgl_stmt <- "
		visualize
			bin(carat,5) as x,
			price as y
		from diamonds
		using boxes
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("boxplot with default collection", p)
})

test_that("uses subquery as a data source", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y,
			cyl as color
		from (
			select
				hp,
				mpg,
				cyl::varchar as cyl
			from cars
			where cyl = 4 or cyl = 6
		)
		using points
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("subquery data source", p)
})

test_that("generates plot with jittered points", {
  set.seed(0)
  sgl_stmt <- "
		visualize
			boolean as x
		from synth
		using jittered points
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("jittered points", p)
})

test_that("generates linear regression line plot", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using regression lines
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("linear regression line plot", p)
})

test_that("generates regression line layered on scatterplot", {
  sgl_stmt <- "
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
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("scatterplot with regression line", p)
})

test_that("generates plot from layered geom expressions", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y
		from cars
		using (
			points
			layer
			regression line
		)
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("plot from layered geom expressions", p)
})

test_that("generates plot with date and timestamp layered", {
  sgl_stmt <- "
		visualize
			day as x,
			number as y
		from synth
		using line

		layer

		visualize
			ts_col as x,
			number as y
		from (
			select
				cast(day + interval '10 years' as timestamp) as ts_col,
				number
			from synth
		)
		using line
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("plot with date and timestamp layered", p)
})

test_that("generates plot with log scales", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y
		from cars
		using (
			points
			layer
			regression line
		)

		scale by
			log(x),
			log(y)
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("plot with log scales", p)
})

test_that("generates plot with ln scales", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y
		from cars
		using (
			points
			layer
			regression line
		)
		scale by
			ln(x),
			ln(y)
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("plot with ln scales", p)
})

test_that("generates bar chart in polar coordinates", {
  sgl_stmt <- "
    visualize
      letter as theta,
      number as r,
			boolean as color
    from synth
    using bars
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("bar chart in polar coordinates", p)
})

test_that("generates pie chart", {
  sgl_stmt <- "
    visualize
      count(*) as theta,
			cut as color
    from diamonds
		group by
			cut
    using bars
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("pie chart", p)
})

test_that("generates one dimensional point plot", {
  sgl_stmt <- "
    visualize
			mpg as x
    from cars
    using points
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("one dimensional point plot", p)
})

test_that("generates scatterplot with size aesthetic", {
  sgl_stmt <- "
    visualize
			hp as x,
			mpg as y,
			cyl as size
    from cars
    using points
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("scatterplot with size aesthetic", p)
})

test_that("generates histogram with horizontal bars", {
  sgl_stmt <- "
    visualize
			bin(mpg) as y,
			count(*) as x
    from cars
		group by
			bin(mpg)
    using horizontal bars
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("histogram with horizontal bars", p)
})

test_that("generates histogram with non-default num bins", {
  sgl_stmt <- "
    visualize
			bin(mpg, 10) as x,
			count(*) as y
    from cars
		group by
			bin(mpg, 10)
    using bars
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("histogram with non-default num bins", p)
})

test_that("generates bar chart for avg log-hp per cyl", {
  sgl_stmt <- "
		visualize
			cyl as x,
			avg(hp) as y
		from cars
		group by
			cyl
		using bars

		scale by
			log(y)
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("bar chart avg log-hp per cyl", p)
})

test_that("generates faceted scatterplot", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y
		from (
			select
				*,
				cast(cyl as varchar) as cyl_cat
			from cars
		)
		using points
		facet by
			cyl_cat
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("faceted scatterplot", p)
})

test_that("generates plot faceted on non-categorical column", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y
		from cars
		using points
		facet by
			cyl
  "

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("non-categorical facet", p)
})

test_that("generates plot with explicit titles", {
  sgl_stmt <- "
		visualize
			hp as x,
			mpg as y,
			cyl as color
		from cars
		using points

		title
			x as 'Horsepower',
			y as 'Miles Per Gallon',
			color as 'Cylinders'
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("plot with explicit titles", p)
})

test_that("generates plot regardless of case", {
  sgl_stmt <- "
		VisualiZe
			hp aS X,
			mpg As y
		FrOM cars
		usinG PoiNts
	"

  p <- dbGetPlot(test_con, sgl_stmt)

  vdiffr::expect_doppelganger("case insensitive plot", p)
})
