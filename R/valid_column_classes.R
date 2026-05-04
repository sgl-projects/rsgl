valid_column_class <- function(column, df) {
  if (column == "*") {
    return()
  }
  col <- df[[column]]
  if (
    is_numerical_col(col) ||
      is_categorical_col(col) ||
      is_temporal_col(col)
  ) {
    return()
  }
  unformatted_msg <- paste(
    "Error: unknown SGL type classification",
    "(numerical, categorical, or temporal)",
    "for column '%s.'"
  )
  err_msg <- sprintf(unformatted_msg, column)
  stop(err_msg)
}

valid_column_classes <- function(layer, df) {
  all_col_exprs <- c(
    layer$aes_mappings,
    layer$groupings,
    layer$collections
  )
  purrr::map(
    all_col_exprs,
    function(col_expr) valid_column_class(col_expr$column, df)
  )
}