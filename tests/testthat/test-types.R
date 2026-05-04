df_with_supported_r_classes <- function() {
  df <- data.frame(
    numeric_col = 1.2,
    logical_col = TRUE,
    Date_col = as.Date("2025-01-01"),
    integer_col = 1L,
    difftime_col = lubridate::make_difftime(1),
    POSIXct_col = as.POSIXct("2025-01-01 00:00:00"),
    character_col = "1",
    factor_col = as.factor("a")
  )

  stopifnot(
    class(df$numeric_col) == "numeric",
    class(df$logical_col) == "logical",
    class(df$Date_col) == "Date",
    class(df$integer_col) == "integer",
    class(df$difftime_col) == "difftime",
    class(df$POSIXct_col)[1] == "POSIXct",
    class(df$character_col) == "character",
    class(df$factor_col) == "factor"
  )

  df
}

test_that("is_numerical_col determines whether column is numerical", {
  df <- df_with_supported_r_classes()

  expect_equal(is_numerical_col(df$numeric_col), TRUE)
  expect_equal(is_numerical_col(df$logical_col), FALSE)
  expect_equal(is_numerical_col(df$Date_col), FALSE)
  expect_equal(is_numerical_col(df$integer_col), TRUE)
  expect_equal(is_numerical_col(df$difftime_col), TRUE)
  expect_equal(is_numerical_col(df$POSIXct_col), FALSE)
  expect_equal(is_numerical_col(df$character_col), FALSE)
  expect_equal(is_numerical_col(df$factor_col), FALSE)
})

test_that("is_categorical_col determines whether column is categorical", {
  df <- df_with_supported_r_classes()

  expect_equal(is_categorical_col(df$numeric_col), FALSE)
  expect_equal(is_categorical_col(df$logical_col), TRUE)
  expect_equal(is_categorical_col(df$Date_col), FALSE)
  expect_equal(is_categorical_col(df$integer_col), FALSE)
  expect_equal(is_categorical_col(df$difftime_col), FALSE)
  expect_equal(is_categorical_col(df$POSIXct_col), FALSE)
  expect_equal(is_categorical_col(df$character_col), TRUE)
  expect_equal(is_categorical_col(df$factor_col), TRUE)
})

test_that("is_temporal_col determines whether column is temporal", {
  df <- df_with_supported_r_classes()

  expect_equal(is_temporal_col(df$numeric_col), FALSE)
  expect_equal(is_temporal_col(df$logical_col), FALSE)
  expect_equal(is_temporal_col(df$Date_col), TRUE)
  expect_equal(is_temporal_col(df$integer_col), FALSE)
  expect_equal(is_temporal_col(df$difftime_col), FALSE)
  expect_equal(is_temporal_col(df$POSIXct_col), TRUE)
  expect_equal(is_temporal_col(df$character_col), FALSE)
  expect_equal(is_temporal_col(df$factor_col), FALSE)
})

test_that("is_numerical_mapping returns true for count star", {
  df <- df_with_supported_r_classes()
  rgs <- sgl_to_rgs("
		visualize
			bin(numeric_col) as x,
			count(*) as y
		from all_classes
		group by
			bin(numeric_col)
		using points
	")

  expect_equal(is_numerical_mapping(rgs$layers[[1]], df, "y"), TRUE)
})

test_that(
  "is_numerical_mapping determines whether mapping to column is numerical",
  {
    df <- df_with_supported_r_classes()
    rgs <- sgl_to_rgs("
			visualize
				numeric_col as x,
				logical_col as y,
				Date_col as color
			from all_classes
			using points
		")

    expect_equal(is_numerical_mapping(rgs$layers[[1]], df, "x"), TRUE)
    expect_equal(is_numerical_mapping(rgs$layers[[1]], df, "y"), FALSE)
    expect_equal(is_numerical_mapping(rgs$layers[[1]], df, "color"), FALSE)
  }
)

test_that("is_categorical_mapping returns false for count star", {
  df <- df_with_supported_r_classes()
  rgs <- sgl_to_rgs("
		visualize
			bin(numeric_col) as x,
			count(*) as y
		from all_classes
		group by
			bin(numeric_col)
		using points
	")

  expect_equal(is_categorical_mapping(rgs$layers[[1]], df, "y"), FALSE)
})

test_that(
  "is_categorical_mapping determines whether mapping to column is categorical",
  {
    df <- df_with_supported_r_classes()
    rgs <- sgl_to_rgs("
			visualize
				numeric_col as x,
				logical_col as y,
				Date_col as color
			from all_classes
			using points
		")

    expect_equal(is_categorical_mapping(rgs$layers[[1]], df, "x"), FALSE)
    expect_equal(is_categorical_mapping(rgs$layers[[1]], df, "y"), TRUE)
    expect_equal(is_categorical_mapping(rgs$layers[[1]], df, "color"), FALSE)
  }
)

test_that("is_temporal_mapping returns false for count star", {
  df <- df_with_supported_r_classes()
  rgs <- sgl_to_rgs("
		visualize
			bin(numeric_col) as x,
			count(*) as y
		from all_classes
		group by
			bin(numeric_col)
		using points
	")

  expect_equal(is_temporal_mapping(rgs$layers[[1]], df, "y"), FALSE)
})

test_that(
  "is_temporal_mapping determines whether mapping to column is temporal",
  {
    df <- df_with_supported_r_classes()
    rgs <- sgl_to_rgs("
			visualize
				numeric_col as x,
				logical_col as y,
				Date_col as color
			from all_classes
			using points
		")

    expect_equal(is_temporal_mapping(rgs$layers[[1]], df, "x"), FALSE)
    expect_equal(is_temporal_mapping(rgs$layers[[1]], df, "y"), FALSE)
    expect_equal(is_temporal_mapping(rgs$layers[[1]], df, "color"), TRUE)
  }
)

test_that("is_binned_mapping determines whether mapping is binned", {
  rgs <- sgl_to_rgs("
		visualize
			bin(numeric_col) as x,
			count(*) as y,
			logical_col as color
		from all_classes
		group by
			bin(numeric_col),
			logical_col
		using bars
	")

  layer <- rgs$layers[[1]]
  expect_equal(is_binned_mapping(layer, "x"), TRUE)
  expect_equal(is_binned_mapping(layer, "y"), FALSE)
  expect_equal(is_binned_mapping(layer, "color"), FALSE)
})

test_that("type_classifications raises error if table doesn't exist", {
  expect_error(
    type_classifications(test_con, "not_a_table"),
    "Error: Table with name not_a_table does not exist!"
  )
})

test_that("type_classifications returns correct classes for table cols", {
  DBI::dbBegin(test_con)
  withr::defer(DBI::dbRollback(test_con))
  DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")

  actual <- type_classifications(test_con, "synth")

  expected <- data.frame(
    column_name = c(
      "letter", "number", "day",
      "day_and_time", "boolean", "blob_col"
    ),
    column_class = c(
      "categorical", "numerical", "temporal",
      "temporal", "categorical", "unknown"
    )
  )

  expect_equal(actual, expected)
})

test_that("type_classifications handles table names needing quoting", {
  DBI::dbBegin(test_con)
  withr::defer(DBI::dbRollback(test_con))
  DBI::dbExecute(
    test_con,
    'create table "weird-name" (a INTEGER, b VARCHAR)'
  )

  actual <- type_classifications(test_con, "weird-name")

  expected <- data.frame(
    column_name = c("a", "b"),
    column_class = c("numerical", "categorical")
  )
  expect_equal(actual, expected)
})
