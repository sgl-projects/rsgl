new_sgl_cta_avg <- function() {
  new_sgl_cta(class = "sgl_cta_avg")
}

#' @export
cta_fn_name.sgl_cta_avg <- function(cta) {
  "avg"
}

#' @export
valid_cta.sgl_cta_avg <- function(cta, col_expr, df) {
  col_name <- col_expr$column
  avg_fn_name <- cta_fn_name(cta)
  if (col_name == "*") {
    stop("Error: '*' cannot be used with the avg function.")
  }

  col <- df[[col_name]]
  if (!is_numerical_col(col)) {
    err_msg <- sprintf(
      "Error: %s function can only be applied to numerical columns.",
      avg_fn_name
    )
    stop(err_msg)
  }

  raise_if_arg_present(col_expr)
}

#' @export
is_aggregation.sgl_cta_avg <- function(cta) {
  TRUE
}

#' @export
is_transformation.sgl_cta_avg <- function(cta) {
  FALSE
}

#' @export
agg_col_name.sgl_cta_avg <- function(cta, col_expr, scale) {
  if (is.null(scale)) {
    scale_nm <- "linear"
  } else {
    scale_nm <- scale_name(scale)
  }
  sprintf(
    "rsgl.%s.%s.%s",
    scale_nm,
    cta_fn_name(cta),
    col_expr$column
  )
}

#' @export
agg_col_expr.sgl_cta_avg <- function(cta, col_expr, scale) {
  if (is.null(scale)) {
    expr_str <- sprintf("mean(%s)", col_expr$column)
  } else {
    expr_str <- sprintf(
      "mean(rsgl.%s.%s)",
      scale_name(scale),
      col_expr$column
    )
  }
  rlang::parse_expr(expr_str)
}

#' @export
expr_text.sgl_cta_avg <- function(cta, col_expr) {
  sprintf(
    "%s(%s)",
    cta_fn_name(cta),
    col_expr$column
  )
}
