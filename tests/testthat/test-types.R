df_with_supported_r_classes <- function() {
  df <- data.frame(
    numeric_col = 1.2,
    logical_col = TRUE,
    Date_col = as.Date("2025-01-01"),
    integer_col = 1L,
    difftime_col = lubridate::make_difftime(1),
    POSIXct_col = as.POSIXct("2025-01-01 00:00:00"),
    character_col = "1"
  )

  stopifnot(
    class(df$numeric_col) == "numeric",
    class(df$logical_col) == "logical",
    class(df$Date_col) == "Date",
    class(df$integer_col) == "integer",
    class(df$difftime_col) == "difftime",
    class(df$POSIXct_col)[1] == "POSIXct",
    class(df$character_col) == "character"
  )

  df
}

test_that("create_tbl_stmt_for_type_test gives correct statement", {
  col_types <- c("VARCHAR", "INTEGER")

  actual_stmt <- create_tbl_stmt_for_type_test(col_types)

  expected_stmt <- "create table type_test (
	\"COL_VARCHAR\" VARCHAR,
	\"COL_INTEGER\" INTEGER
)"
  expect_equal(actual_stmt, expected_stmt)
})

test_that("valid_duckdb_type doesnt raise error for valid types", {
  col_types <- c("VARCHAR", "INTEGER")

  expect_no_error(
    valid_duckdb_types(col_types)
  )
})

test_that("valid_duckdb_type raises error for invalid type", {
  col_types <- c("VARCHAR", "INVALID_TYPE")

  expect_error(
    valid_duckdb_types(col_types),
    "Type with name INVALID_TYPE does not exist!",
    fixed = FALSE
  )
})

test_that("types_are_supported_by_duckdb doesnt raise an error", {
  expect_no_error(types_are_supported_by_duckdb())
})

test_that("rsgl types map to R classes as expected", {
  actual_map <- map_rsgl_types_to_r_classes()

  expected_map <- list(
    numeric = c(
      "BIGINT", "INT8", "LONG",
      "DECIMAL", "NUMERIC",
      "DOUBLE", "FLOAT8",
      "FLOAT", "FLOAT4", "REAL",
      "HUGEINT",
      "UBIGINT",
      "UHUGEINT",
      "UINTEGER"
    ),
    logical = c("BOOLEAN", "BOOL", "LOGICAL"),
    Date = c("DATE"),
    integer = c(
      "INTEGER", "INT4", "INT", "SIGNED",
      "SMALLINT", "INT2", "SHORT",
      "TINYINT", "INT1",
      "USMALLINT",
      "UTINYINT"
    ),
    difftime = c("INTERVAL", "TIME"),
    POSIXct = c(
      "TIMESTAMP WITH TIME ZONE", "TIMESTAMPTZ",
      "TIMESTAMP", "DATETIME"
    ),
    character = c(
      "UUID",
      "VARCHAR", "CHAR", "BPCHAR", "TEXT", "STRING"
    )
  )
  expect_equal(actual_map, expected_map)
})

test_that("is_numerical_col determines whether column is numerical", {
  df <- df_with_supported_r_classes()

  expect_equal(is_numerical_col(df$numeric_col), TRUE)
  expect_equal(is_numerical_col(df$logical_col), FALSE)
  expect_equal(is_numerical_col(df$Date_col), FALSE)
  expect_equal(is_numerical_col(df$integer_col), TRUE)
  expect_equal(is_numerical_col(df$difftime_col), TRUE)
  expect_equal(is_numerical_col(df$POSIXct_col), FALSE)
  expect_equal(is_numerical_col(df$character_col), FALSE)
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
})

test_that(
  "rsgl_types returns all supported types when no class is provided",
  {
    actual_types <- rsgl_types()

    expected_types <- c(
      "BIGINT",
      "INT8",
      "LONG",
      "BOOLEAN",
      "BOOL",
      "LOGICAL",
      "DATE",
      "DECIMAL",
      "NUMERIC",
      "DOUBLE",
      "FLOAT8",
      "FLOAT",
      "FLOAT4",
      "REAL",
      "HUGEINT",
      "INTEGER",
      "INT4",
      "INT",
      "SIGNED",
      "INTERVAL",
      "SMALLINT",
      "INT2",
      "SHORT",
      "TIME",
      "TIMESTAMP WITH TIME ZONE",
      "TIMESTAMPTZ",
      "TIMESTAMP",
      "DATETIME",
      "TINYINT",
      "INT1",
      "UBIGINT",
      "UHUGEINT",
      "UINTEGER",
      "USMALLINT",
      "UTINYINT",
      "UUID",
      "VARCHAR",
      "CHAR",
      "BPCHAR",
      "TEXT",
      "STRING"
    )

    expect_equal(actual_types, expected_types)
  }
)

test_that("rsgl_types returns all numerical types", {
  actual_types <- rsgl_types("numerical")

  expected_types <- c(
    "BIGINT",
    "INT8",
    "LONG",
    "DECIMAL",
    "NUMERIC",
    "DOUBLE",
    "FLOAT8",
    "FLOAT",
    "FLOAT4",
    "REAL",
    "HUGEINT",
    "INTEGER",
    "INT4",
    "INT",
    "SIGNED",
    "INTERVAL",
    "SMALLINT",
    "INT2",
    "SHORT",
    "TIME",
    "TINYINT",
    "INT1",
    "UBIGINT",
    "UHUGEINT",
    "UINTEGER",
    "USMALLINT",
    "UTINYINT"
  )

  expect_equal(actual_types, expected_types)
})

test_that("rsgl_types returns all categorical types", {
  actual_types <- rsgl_types("categorical")

  expected_types <- c(
    "BOOLEAN",
    "BOOL",
    "LOGICAL",
    "UUID",
    "VARCHAR",
    "CHAR",
    "BPCHAR",
    "TEXT",
    "STRING"
  )

  expect_equal(actual_types, expected_types)
})


test_that("rsgl_types returns all temporal types", {
  actual_types <- rsgl_types("temporal")

  expected_types <- c(
    "DATE",
    "TIMESTAMP WITH TIME ZONE",
    "TIMESTAMPTZ",
    "TIMESTAMP",
    "DATETIME"
  )

  expect_equal(actual_types, expected_types)
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
