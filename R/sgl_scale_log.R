new_sgl_scale_log <- function() {
  new_sgl_scale(class = "sgl_scale_log")
}

#' @export
scale_name.sgl_scale_log <- function(scale) {
  "log"
}

#' @export
valid_scale.sgl_scale_log <- function(scale, aes, layers, dfs) {
  NextMethod()
  raise_for_non_nums(scale, aes, layers, dfs)
}

#' @export
apply_scale.sgl_scale_log <- function(scale, values) {
  log(values, base = 10)
}

#' @export
apply_scale_inverse.sgl_scale_log <- function(scale, values) {
  10^values
}

#' @export
ggplot_scales.sgl_scale_log <- function(scale, aes, rgs) {
  ggplot_continuous_scales("log10", aes, rgs)
}
