new_sgl_cta_count <- function() {
  new_sgl_cta(class = "sgl_cta_count")
}

#' @export
cta_name.sgl_cta_count <- function(cta) {
  "count"
}

#' @export
valid_cta.sgl_cta_count <- function(cta, col_name, df) {
  if (col_name != "*") {
    count_cta_name <- cta_name(cta)
    errmsg <- sprintf(
      "Error: %s can only be applied to *, found %s(%s).",
      count_cta_name,
      count_cta_name,
      col_name
    )
    stop(errmsg)
  }
}

#' @export
is_transformation.sgl_cta_count <- function(cta) {
  FALSE
}
