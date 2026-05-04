new_sgl_cta_avg <- function() {
  new_sgl_cta(class = "sgl_cta_avg")
}

#' @export
cta_name.sgl_cta_avg <- function(cta) {
  "avg"
}

#' @export
valid_cta.sgl_cta_avg <- function(cta, col_expr, df) {
  col_name <- col_expr$column
  avg_cta_name <- cta_name(cta)
  if (col_name == "*") {
    stop("Error: '*' cannot be used with the avg function.")
  }

  col <- df[[col_name]]
  if (!is_numerical_col(col)) {
    err_msg <- sprintf(
      "Error: %s function can only be applied to numerical columns.",
      avg_cta_name
    )
    stop(err_msg)
  }

  if ("arg" %in% names(col_expr)) {
    err_msg <- sprintf(
      "Error: %s function received unexpected argument.",
      avg_cta_name
    )
    stop(err_msg)
  }
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
    cta_name(cta),
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
