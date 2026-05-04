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

#' Get SGL type classificatons for columns in a table
#'
#' `type_classifications` takes a DBI connection and a table name
#' and returns the SGL type classifications (numerical, categorical,
#' or temporal) of the table's columns.
#'
#' @param con A DBI connection
#' @param table_name The name of a table
#'
#' @return A dataframe listing the SGL type classification of each column.
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "iris", iris)
#' type_classifications(con, "iris")
#'
#' @export
type_classifications <- function(con, table_name) {
  sql <- sprintf(
    "select * from %s limit 1",
    DBI::dbQuoteIdentifier(con, table_name)
  )
  df <- DBI::dbGetQuery(con, sql)

  col_class <- function(col) {
    if (is_numerical_col(col)) {
      "numerical"
    } else if (is_categorical_col(col)) {
      "categorical"
    } else if (is_temporal_col(col)) {
      "temporal"
    } else {
      "unknown"
    }
  }
  classes <- purrr::map_chr(
    df, col_class
  )

  data.frame(
    column_name = names(df),
    column_class = unname(classes)
  )
}
