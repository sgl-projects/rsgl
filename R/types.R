rsgl_types <- function(class) {
  all_types <- c(
    "BIGINT",
    "INT8", # BIGINT alias
    "LONG", # BIGINT alias
    "BOOLEAN",
    "BOOL", # BOOLEAN alias
    "LOGICAL", # BOOLEAN alias
    "DATE",
    "DECIMAL",
    "TEST_ENUM",
    "NUMERIC", # DECIMAL alias
    "DOUBLE",
    "FLOAT8", # DOUBLE alias
    "FLOAT",
    "FLOAT4", # FLOAT alias
    "REAL", # FLOAT alias
    "HUGEINT",
    "INTEGER",
    "INT4", # INTEGER alias
    "INT", # INTEGER alias
    "SIGNED", # INTEGER alias
    "INTERVAL",
    "SMALLINT",
    "INT2", # SMALLINT alias
    "SHORT", # SMALLINT alias
    "TIME",
    "TIMESTAMP WITH TIME ZONE",
    "TIMESTAMPTZ", # TIMESTAMP WITH TIME ZONE alias
    "TIMESTAMP",
    "DATETIME", # TIMESTAMP alias
    "TINYINT",
    "INT1", # TINYINT alias
    "UBIGINT",
    "UHUGEINT",
    "UINTEGER",
    "USMALLINT",
    "UTINYINT",
    "UUID",
    "VARCHAR",
    "CHAR", # VARCHAR alias
    "BPCHAR", # VARCHAR alias
    "TEXT", # VARCHAR alias
    "STRING" # VARCHAR alias
  )
  if (missing(class)) {
    return(all_types)
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  enum_type_stmt <- create_enum_stmt_for_type_test()
  DBI::dbExecute(con, enum_type_stmt)
  tbl_stmt <- create_tbl_stmt_for_type_test(all_types)
  DBI::dbExecute(con, tbl_stmt)
  df <- DBI::dbGetQuery(con, "select * from type_test")
  class_check_fn_list <- list(
    "numerical" = is_numerical_col,
    "categorical" = is_categorical_col,
    "temporal" = is_temporal_col
  )
  class_check_fn <- class_check_fn_list[[class]]
  types_in_class <- c()
  for (col_name in colnames(df)) {
    if (class_check_fn(df[[col_name]])) {
      duckdb_type <- substring(col_name, 5)
      types_in_class <- c(types_in_class, duckdb_type)
    }
  }
  types_in_class
}

create_enum_stmt_for_type_test <- function() {
  "create type TEST_ENUM as ENUM ('test')"
}

create_tbl_stmt_for_type_test <- function(types) {
  column_defs <- paste0("\t\"COL_", types, "\" ", types, collapse = ",\n")
  paste0("create table type_test (\n", column_defs, "\n)")
}

valid_duckdb_types <- function(types) {
  con <- DBI::dbConnect(duckdb::duckdb())
  enum_type_stmt <- create_enum_stmt_for_type_test()
  DBI::dbExecute(con, enum_type_stmt)
  tbl_stmt <- create_tbl_stmt_for_type_test(types)
  DBI::dbExecute(con, tbl_stmt)
}

types_are_supported_by_duckdb <- function() {
  valid_duckdb_types(rsgl_types())
}

map_rsgl_types_to_r_classes <- function() {
  map <- list()
  con <- DBI::dbConnect(duckdb::duckdb())
  enum_type_stmt <- create_enum_stmt_for_type_test()
  DBI::dbExecute(con, enum_type_stmt)
  tbl_stmt <- create_tbl_stmt_for_type_test(rsgl_types())
  DBI::dbExecute(con, tbl_stmt)
  df <- DBI::dbGetQuery(con, "select * from type_test")
  for (col_name in colnames(df)) {
    duckdb_type <- substring(col_name, 5)
    r_class <- class(df[[col_name]])[1]
    map[[r_class]] <- c(map[[r_class]], duckdb_type)
  }
  map
}

is_numerical_col <- function(col) {
  numerical_classes <- c("numeric", "integer", "difftime")
  class(col)[1] %in% numerical_classes
}

is_categorical_col <- function(col) {
  categorical_classes <- c("logical", "character", "factor")
  class(col)[1] %in% categorical_classes
}

is_temporal_col <- function(col) {
  temporal_classes <- c("Date", "POSIXct")
  class(col)[1] %in% temporal_classes
}

is_numerical_mapping <- function(layer, df, aes) {
  aes_mapping <- layer$aes_mappings[[aes]]
  if (col_expr_has_cta(aes_mapping, "count")) {
    TRUE
  } else {
    col <- column_from_aes(layer, df, aes)
    is_numerical_col(col)
  }
}

is_categorical_mapping <- function(layer, df, aes) {
  aes_mapping <- layer$aes_mappings[[aes]]
  if (col_expr_has_cta(aes_mapping, "count")) {
    FALSE
  } else {
    col <- column_from_aes(layer, df, aes)
    is_categorical_col(col)
  }
}


is_temporal_mapping <- function(layer, df, aes) {
  aes_mapping <- layer$aes_mappings[[aes]]
  if (col_expr_has_cta(aes_mapping, "count")) {
    FALSE
  } else {
    col <- column_from_aes(layer, df, aes)
    is_temporal_col(col)
  }
}

is_binned_mapping <- function(layer, aes) {
  aes_mapping <- layer$aes_mappings[[aes]]
  if (col_expr_has_cta(aes_mapping, "bin")) {
    TRUE
  } else {
    FALSE
  }
}
