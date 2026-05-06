new_sgl_cta_count <- function() {
  new_sgl_cta(class = "sgl_cta_count")
}

#' @export
cta_name.sgl_cta_count <- function(cta) {
  "count"
}

#' @export
valid_cta.sgl_cta_count <- function(cta, col_expr, df) {
  col_name <- col_expr$column
  count_cta_name <- cta_name(cta)
  if (col_name != "*") {
    err_msg <- sprintf(
      "Error: %s can only be applied to *, found %s(%s).",
      count_cta_name,
      count_cta_name,
      col_name
    )
    stop(err_msg)
  }

  if ("arg" %in% names(col_expr)) {
    err_msg <- sprintf(
      "Error: %s function received unexpected argument.",
      count_cta_name
    )
    stop(err_msg)
  }
}

#' @export
is_aggregation.sgl_cta_count <- function(cta) {
  TRUE
}

#' @export
is_transformation.sgl_cta_count <- function(cta) {
  FALSE
}

#' @export
agg_col_name.sgl_cta_count <- function(cta, col_expr, scale) {
  "rsgl.count"
}

#' @export
agg_col_expr.sgl_cta_count <- function(cta, col_expr, scale) {
  rlang::expr(dplyr::n())
}

#' @export
default_title.sgl_cta_count <- function(cta, col_expr) {
  "count(*)"
}
