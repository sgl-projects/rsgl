new_sgl_scale_linear <- function() {
  new_sgl_scale(class = "sgl_scale_linear")
}

#' @export
scale_name.sgl_scale_linear <- function(scale) {
  "linear"
}

#' @export
valid_scale.sgl_scale_linear <- function(scale, aes, layers, dfs) {
  NextMethod()
  raise_for_non_nums(scale, aes, layers, dfs)
}

#' @export
apply_scale.sgl_scale_linear <- function(scale, values) {
  values
}

#' @export
apply_scale_inverse.sgl_scale_linear <- function(scale, values) {
  values
}

#' @export
ggplot_scales.sgl_scale_linear <- function(scale, aes, rgs) {
  ggplot_continuous_scales("identity", aes, rgs)
}
