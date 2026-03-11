new_sgl_cta_identity <- function() {
  new_sgl_cta(class = "sgl_cta_identity")
}

#' @export
valid_cta.sgl_cta_identity <- function(cta, col_name, df) {
  if (col_name == "*") {
    stop("Error: '*' can only be used inside an aggregation function")
  }
}

#' @export
is_transformation.sgl_cta_identity <- function(cta) {
  FALSE
}
