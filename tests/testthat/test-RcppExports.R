test_that("adds single aes mapping", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  aes_mappings <- rgs$layers[[1]]$aes_mappings
  actual_x_mapping <- aes_mappings$x
  expect_equal(names(aes_mappings), "x")
  expect_setequal(names(actual_x_mapping), c("column", "cta"))
  expect_equal(actual_x_mapping$column, "col_1")
  expect_equal(actual_x_mapping$cta, new_sgl_cta_identity())
})

test_that("adds multiple aes mappings", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  aes_mappings <- rgs$layers[[1]]$aes_mappings
  actual_x_mapping <- aes_mappings$x
  actual_y_mapping <- aes_mappings$y
  expect_setequal(names(aes_mappings), c("x", "y"))
  expect_equal(actual_x_mapping$column, "col_1")
  expect_equal(actual_x_mapping$cta, new_sgl_cta_identity())
  expect_equal(actual_y_mapping$column, "col_2")
  expect_equal(actual_y_mapping$cta, new_sgl_cta_identity())
})

test_that("adds column transformation", {
  sgl_stmt <- "
		visualize
			bin(col_1) as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  aes_mappings <- rgs$layers[[1]]$aes_mappings
  actual_x_mapping <- aes_mappings$x
  expect_equal(names(aes_mappings), "x")
  expect_setequal(names(actual_x_mapping), c("column", "cta"))
  expect_equal(actual_x_mapping$column, "col_1")
  expect_equal(actual_x_mapping$cta, new_sgl_cta_bin())
})

test_that("adds aes mapping with function arg", {
  sgl_stmt <- "
		visualize
			bin(col_1, 5) as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  aes_mappings <- rgs$layers[[1]]$aes_mappings
  actual_x_mapping <- aes_mappings$x
  expect_equal(names(aes_mappings), "x")
  expect_setequal(names(actual_x_mapping), c("column", "cta", "arg"))
  expect_equal(actual_x_mapping$column, "col_1")
  expect_equal(actual_x_mapping$cta, new_sgl_cta_bin())
  expect_equal(actual_x_mapping$arg, 5)
})

test_that("adds source sql query for table name", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  actual_source_sql_query <- rgs$layers[[1]]$source_sql_query
  expected_source_sql_query <- "select * from table_1"
  expect_equal(actual_source_sql_query, expected_source_sql_query)
})

test_that("adds source sql query for subquery", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from (
			select *
			from table_1
			where col_2='a'
		)
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  actual_source_sql_query <- rgs$layers[[1]]$source_sql_query
  expected_source_sql_query <- "
			select *
			from table_1
			where col_2='a'
		"
  expect_equal(actual_source_sql_query, expected_source_sql_query)
})

test_that("adds point geom", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  geom_expr <- rgs$layers[[1]]$geom_expr
  actual_geom <- geom_expr$geom
  expected_geom <- new_sgl_geom_point()
  expect_equal(actual_geom, expected_geom)
  actual_qual <- geom_expr$qual
  expected_qual <- "default"
  expect_equal(actual_qual, expected_qual)
})

test_that("adds bar geom", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using bars
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  geom_expr <- rgs$layers[[1]]$geom_expr
  actual_geom <- geom_expr$geom
  expected_geom <- new_sgl_geom_bar()
  expect_equal(actual_geom, expected_geom)
  actual_qual <- geom_expr$qual
  expected_qual <- "default"
  expect_equal(actual_qual, expected_qual)
})

test_that("adds line geom", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using line
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  geom_expr <- rgs$layers[[1]]$geom_expr
  actual_geom <- geom_expr$geom
  expected_geom <- new_sgl_geom_line()
  expect_equal(actual_geom, expected_geom)
  actual_qual <- geom_expr$qual
  expected_qual <- "default"
  expect_equal(actual_qual, expected_qual)
})

test_that("adds box geom", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using box
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  geom_expr <- rgs$layers[[1]]$geom_expr
  actual_geom <- geom_expr$geom
  expected_geom <- new_sgl_geom_box()
  expect_equal(actual_geom, expected_geom)
  actual_qual <- geom_expr$qual
  expected_qual <- "default"
  expect_equal(actual_qual, expected_qual)
})

test_that("adds geom with qualifier", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using regression line
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  geom_expr <- rgs$layers[[1]]$geom_expr
  actual_geom <- geom_expr$geom
  expected_geom <- new_sgl_geom_line()
  expect_equal(actual_geom, expected_geom)
  actual_qual <- geom_expr$qual
  expected_qual <- "regression"
  expect_equal(actual_qual, expected_qual)
})

test_that("doesn't add groupings if group by clause omitted", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  layer <- rgs$layers[[1]]
  expect_false("groupings" %in% names(rgs))
})

test_that("adds grouping for single column", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			count(*) as y
		from table_1
		group by
			col_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  groupings <- rgs$layers[[1]]$groupings
  expect_equal(length(groupings), 1)
  grouping <- groupings[[1]]
  expect_setequal(names(grouping), c("column", "cta"))
  expect_equal(grouping$column, "col_1")
  expect_equal(grouping$cta, new_sgl_cta_identity())
})

test_that("adds grouping for single transformed column", {
  sgl_stmt <- "
		visualize
			bin(col_1) as x,
			count(*) as y
		from table_1
		group by
			bin(col_1)
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  groupings <- rgs$layers[[1]]$groupings
  expect_equal(length(groupings), 1)
  grouping <- groupings[[1]]
  expect_setequal(names(grouping), c("column", "cta"))
  expect_equal(grouping$column, "col_1")
  expect_equal(grouping$cta, new_sgl_cta_bin())
})

test_that("adds grouping with function arg", {
  sgl_stmt <- "
		visualize
			bin(col_1, 5) as x,
			count(*) as y
		from table_1
		group by
			bin(col_1, 5)
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  groupings <- rgs$layers[[1]]$groupings
  expect_equal(length(groupings), 1)
  grouping <- groupings[[1]]
  expect_setequal(names(grouping), c("column", "cta", "arg"))
  expect_equal(grouping$column, "col_1")
  expect_equal(grouping$cta, new_sgl_cta_bin())
  expect_equal(grouping$arg, 5)
})

test_that("adds groupings for multiple grouping expressions", {
  sgl_stmt <- "
		visualize
			bin(col_1) as x,
			count(*) as y,
			col_2 as color
		from table_1
		group by
			bin(col_1),
			col_2
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  groupings <- rgs$layers[[1]]$groupings
  expect_equal(length(groupings), 2)
  first_grouping <- groupings[[2]]
  second_grouping <- groupings[[1]]
  expect_setequal(names(first_grouping), c("column", "cta"))
  expect_equal(first_grouping$column, "col_1")
  expect_equal(first_grouping$cta, new_sgl_cta_bin())
  expect_setequal(names(second_grouping), c("column", "cta"))
  expect_equal(second_grouping$column, "col_2")
  expect_equal(second_grouping$cta, new_sgl_cta_identity())
})

test_that("doesn't add collections if collect by clause omitted", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  layer <- rgs$layers[[1]]
  expect_false("collections" %in% names(rgs))
})

test_that("adds collection for single column", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		collect by
			col_3
		using lines
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  collections <- rgs$layers[[1]]$collections
  expect_equal(length(collections), 1)
  collection <- collections[[1]]
  expect_setequal(names(collection), c("column", "cta"))
  expect_equal(collection$column, "col_3")
  expect_equal(collection$cta, new_sgl_cta_identity())
})

test_that("adds collection for single transformed column", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		collect by
			bin(col_3)
		using lines
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  collections <- rgs$layers[[1]]$collections
  expect_equal(length(collections), 1)
  collection <- collections[[1]]
  expect_setequal(names(collection), c("column", "cta"))
  expect_equal(collection$column, "col_3")
  expect_equal(collection$cta, new_sgl_cta_bin())
})

test_that("adds collection with function arg", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		collect by
			bin(col_3, 5)
		using lines
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  collections <- rgs$layers[[1]]$collections
  expect_equal(length(collections), 1)
  collection <- collections[[1]]
  expect_setequal(names(collection), c("column", "cta", "arg"))
  expect_equal(collection$column, "col_3")
  expect_equal(collection$cta, new_sgl_cta_bin())
  expect_equal(collection$arg, 5)
})

test_that("adds collection for multiple collection expressions", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		collect by
			bin(col_3),
			col_4
		using lines
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  collections <- rgs$layers[[1]]$collections
  expect_equal(length(collections), 2)
  first_collection <- collections[[2]]
  second_collection <- collections[[1]]
  expect_setequal(names(first_collection), c("column", "cta"))
  expect_equal(first_collection$column, "col_3")
  expect_equal(first_collection$cta, new_sgl_cta_bin())
  expect_setequal(names(second_collection), c("column", "cta"))
  expect_equal(second_collection$column, "col_4")
  expect_equal(second_collection$cta, new_sgl_cta_identity())
})

test_that("adds one layer for single layer", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_equal(length(rgs$layers), 1)
})

test_that("adds multiple layers", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		group by
			col_1
		using points

		layer

		visualize
			col_2 as y
		from table_2
		collect by
			col_2
		using line
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  first_layer <- rgs$layers[[1]]
  second_layer <- rgs$layers[[2]]

  expect_equal(length(rgs$layers), 2)
  expect_equal(first_layer$aes_mappings$x$column, "col_1")
  expect_equal(first_layer$source_sql_query, "select * from table_1")
  expect_equal(first_layer$geom_expr$geom, new_sgl_geom_point())
  expect_equal(first_layer$groupings[[1]]$column, "col_1")
  expect_false("collections" %in% names(first_layer))
  expect_equal(second_layer$aes_mappings$y$column, "col_2")
  expect_equal(second_layer$source_sql_query, "select * from table_2")
  expect_equal(second_layer$geom_expr$geom, new_sgl_geom_line())
  expect_false("groupings" %in% names(second_layer))
  expect_equal(second_layer$collections[[1]]$column, "col_2")
})

test_that("adds multiple layers for layered geom exprs", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using (
			points
			layer
			regression line
		)
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  first_layer <- rgs$layers[[1]]
  second_layer <- rgs$layers[[2]]

  expected_column <- "col_1"
  expected_source_sql_query <- "select * from table_1"
  expect_equal(length(rgs$layers), 2)
  expect_equal(first_layer$aes_mappings$x$column, expected_column)
  expect_equal(first_layer$source_sql_query, expected_source_sql_query)
  expect_equal(first_layer$geom_expr$geom, new_sgl_geom_point())
  expect_equal(first_layer$geom_expr$qual, "default")
  expect_equal(second_layer$aes_mappings$x$column, expected_column)
  expect_equal(second_layer$source_sql_query, expected_source_sql_query)
  expect_equal(second_layer$geom_expr$geom, new_sgl_geom_line())
  expect_equal(second_layer$geom_expr$qual, "regression")
})

test_that("adds multiple layers for layered geom exprs and top level layer", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using (
			points
			layer
			regression line
		)

		layer

		visualize
			col_2 as x
		from table_2
		using bars
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  first_layer <- rgs$layers[[1]]
  second_layer <- rgs$layers[[2]]
  third_layer <- rgs$layers[[3]]

  expect_equal(length(rgs$layers), 3)
  expect_equal(first_layer$aes_mappings$x$column, "col_1")
  expect_equal(first_layer$source_sql_query, "select * from table_1")
  expect_equal(first_layer$geom_expr$geom, new_sgl_geom_point())
  expect_equal(first_layer$geom_expr$qual, "default")
  expect_equal(second_layer$aes_mappings$x$column, "col_1")
  expect_equal(second_layer$source_sql_query, "select * from table_1")
  expect_equal(second_layer$geom_expr$geom, new_sgl_geom_line())
  expect_equal(second_layer$geom_expr$qual, "regression")
  expect_equal(third_layer$aes_mappings$x$column, "col_2")
  expect_equal(third_layer$source_sql_query, "select * from table_2")
  expect_equal(third_layer$geom_expr$geom, new_sgl_geom_bar())
  expect_equal(third_layer$geom_expr$qual, "default")
})

test_that("doesn't add scale if none provided", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_false("scales" %in% names(rgs))
})

test_that("adds single scale", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		scale by
			log(x)
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_equal(names(rgs$scales), "x")
  expect_equal(rgs$scales$x, new_sgl_scale_log())
})

test_that("adds multiple scales", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		scale by
			linear(x),
			log(y)
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_setequal(names(rgs$scales), c("x", "y"))
  expect_equal(rgs$scales$x, new_sgl_scale_linear())
  expect_equal(rgs$scales$y, new_sgl_scale_log())
})

test_that("adds scale for multiple layers", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points

		layer

		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using regression line

		scale by
			log(x)
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_equal(names(rgs$scales), "x")
  expect_equal(rgs$scales$x, new_sgl_scale_log())
})

test_that("doesn't add facet if none provided", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_false("facets" %in% names(rgs))
})

test_that("adds facet with default direction", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		facet by
			col_3
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  facets <- rgs$facets
  expect_equal(length(facets), 1)
  facet <- facets[[1]]
  expect_setequal(names(facet), c("column", "direction"))
  expect_equal(facet$column, "col_3")
  expect_equal(facet$direction, "default")
})

test_that("adds facet with horizontal direction", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		facet by
			col_3 horizontally
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  facets <- rgs$facets
  expect_equal(length(facets), 1)
  facet <- facets[[1]]
  expect_setequal(names(facet), c("column", "direction"))
  expect_equal(facet$column, "col_3")
  expect_equal(facet$direction, "horizontal")
})

test_that("adds facet with vertical direction", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		facet by
			col_3 vertically
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  facets <- rgs$facets
  expect_equal(length(facets), 1)
  facet <- facets[[1]]
  expect_setequal(names(facet), c("column", "direction"))
  expect_equal(facet$column, "col_3")
  expect_equal(facet$direction, "vertical")
})

test_that("adds multiple facets correctly", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		facet by
			col_3,
			col_4 horizontally,
			col_5 vertically
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  facets <- rgs$facets
  expect_equal(length(facets), 3)
  first_facet <- facets[[3]]
  second_facet <- facets[[2]]
  third_facet <- facets[[1]]
  expect_setequal(names(first_facet), c("column", "direction"))
  expect_equal(first_facet$column, "col_3")
  expect_equal(first_facet$direction, "default")
  expect_setequal(names(second_facet), c("column", "direction"))
  expect_equal(second_facet$column, "col_4")
  expect_equal(second_facet$direction, "horizontal")
  expect_setequal(names(third_facet), c("column", "direction"))
  expect_equal(third_facet$column, "col_5")
  expect_equal(third_facet$direction, "vertical")
})

test_that("adds facet with multiple layers", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points

		layer

		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using regression line

		facet by
			col_3 horizontally
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  facets <- rgs$facets
  expect_equal(length(facets), 1)
  facet <- facets[[1]]
  expect_setequal(names(facet), c("column", "direction"))
  expect_equal(facet$column, "col_3")
  expect_equal(facet$direction, "horizontal")
})

test_that("doesn't add title if none provided", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  expect_false("titles" %in% names(rgs))
})

test_that("adds title", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		title
			x as 'Column 1'
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  titles <- rgs$titles
  expect_equal(names(titles), "x")
  expect_equal(titles$x, "Column 1")
})

test_that("adds empty string as title", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		title
			x as ''
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  titles <- rgs$titles
  expect_equal(names(titles), "x")
  expect_equal(titles$x, "")
})

test_that("allows escaped single quote in title", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		title
			x as 'X\\'s Title'
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  titles <- rgs$titles
  expect_equal(names(titles), "x")
  expect_equal(titles$x, "X's Title")
})

test_that("adds multiple titles", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		title
			x as 'Column 1',
			y as 'Column 2'
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  titles <- rgs$titles
  expect_setequal(names(titles), c("x", "y"))
  expect_equal(titles$x, "Column 1")
  expect_equal(titles$y, "Column 2")
})

test_that("adds title with multiple layers", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points

		layer

		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using regression line

		title
			x as 'Column 1'
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  titles <- rgs$titles
  expect_equal(names(titles), "x")
  expect_equal(titles$x, "Column 1")
})

test_that("adds multiple graphics clauses", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		using points
		facet by
			col_3
		scale by
			log(x)
		title
			x as 'Column 1'
	"

  rgs <- sgl_to_rgs(sgl_stmt)

  facets <- rgs$facets
  expect_equal(length(facets), 1)
  facet <- facets[[1]]
  expect_setequal(names(facet), c("column", "direction"))
  expect_equal(facet$column, "col_3")
  expect_equal(facet$direction, "default")
  scales <- rgs$scales
  expect_equal(names(scales), "x")
  expect_equal(scales$x, new_sgl_scale_log())
  titles <- rgs$titles
  expect_equal(names(titles), "x")
  expect_equal(titles$x, "Column 1")
})

test_that("raises error for invalid aesthetic", {
  sgl_stmt <- "
		visualize
			col_1 as notanaes
		from table_1
		using points
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid aesthetic name: notanaes\n"
  )
})

test_that("raises error for invalid geom", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using notageom
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid geom name: notageom\n"
  )
})

test_that("raises error for invalid qualifier", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using notaqual points
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid geom qualifier: notaqual\n"
  )
})

test_that("raises error for invalid geom in layered geom exprs", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using (
			notageom
			layer
			points
		)
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid geom name: notageom\n"
  )
})

test_that("raises error for invalid qualifier in layered geom exprs", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using (
			notaqual points
			layer
			line
		)
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid geom qualifier: notaqual\n"
  )
})

test_that("raises error for invalid cta", {
  sgl_stmt <- "
		visualize
			not_a_cta(col_1) as x
		from table_1
		using points
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid CTA: not_a_cta\n"
  )
})

test_that("raises error for invalid cta in grouping expression", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		group by
			not_a_cta(col_1)
		using points
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid CTA: not_a_cta\n"
  )
})

test_that("raises error for invalid cta in collection expression", {
  sgl_stmt <- "
		visualize
			col_1 as x,
			col_2 as y
		from table_1
		collect by
			not_a_cta(col_3)
		using points
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid CTA: not_a_cta\n"
  )
})

test_that("raises error for invalid scale type", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
		scale by
			not_a_scale(x)
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid scale type: not_a_scale\n"
  )
})

test_that("raises error for invalid scale aes", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
		scale by
			log(notanaes)
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid aesthetic name: notanaes\n"
  )
})

test_that("raises error for invalid title aes", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
		title
			notanaes as 'Column 1'
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "Invalid aesthetic name: notanaes\n"
  )
})

test_that("raises error for unquoted title", {
  sgl_stmt <- "
		visualize
			col_1 as x
		from table_1
		using points
		title
			notanaes as Column1
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "syntax error\n"
  )
})

test_that("raises error for general syntax error", {
  sgl_stmt <- "
		visualize
		from geom
	"

  expect_error(
    sgl_to_rgs(sgl_stmt),
    "syntax error\n"
  )
})
