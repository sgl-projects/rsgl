new_sgl_cta_identity <- function() {
  new_sgl_cta(class = "sgl_cta_identity")
}

#' @export
valid_cta.sgl_cta_identity <- function(cta, col_expr, df) {
  if (col_expr$column == "*") {
    stop("Error: '*' can only be used inside an aggregation function")
  }
}

#' @export
is_aggregation.sgl_cta_identity <- function(cta) {
  FALSE
}

#' @export
is_transformation.sgl_cta_identity <- function(cta) {
  FALSE
}

#' @export
default_title.sgl_cta_identity <- function(cta, col_expr) {
  col_expr$column
}
