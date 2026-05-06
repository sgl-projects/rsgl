new_sgl_scale_ln <- function() {
  new_sgl_scale(class = "sgl_scale_ln")
}

#' @export
scale_name.sgl_scale_ln <- function(scale) {
  "ln"
}

#' @export
valid_scale.sgl_scale_ln <- function(scale, aes, layers, dfs) {
  NextMethod()
  raise_for_non_nums(scale, aes, layers, dfs)
}

#' @export
apply_scale.sgl_scale_ln <- function(scale, values) {
  log(values)
}

#' @export
apply_scale_inverse.sgl_scale_ln <- function(scale, values) {
  exp(values)
}

#' @export
ggplot_scales.sgl_scale_ln <- function(scale, aes, rgs) {
  ggplot_continuous_scales("log", aes, rgs)
}
