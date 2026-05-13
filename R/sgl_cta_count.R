new_sgl_cta_count <- function() {
  new_sgl_cta(class = "sgl_cta_count")
}

#' @export
cta_fn_name.sgl_cta_count <- function(cta) {
  "count"
}

#' @export
valid_cta.sgl_cta_count <- function(cta, col_expr, df) {
  count_fn_name <- cta_fn_name(cta)
  col_name <- col_expr$column
  if (col_name != "*") {
    err_msg <- sprintf(
      "Error: %s can only be applied to *, found %s(%s).",
      count_fn_name,
      count_fn_name,
      col_name
    )
    stop(err_msg)
  }

  raise_if_arg_present(col_expr)
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
expr_text.sgl_cta_count <- function(cta, col_expr) {
  sprintf(
    "%s(*)",
    cta_fn_name(cta)
  )
}
