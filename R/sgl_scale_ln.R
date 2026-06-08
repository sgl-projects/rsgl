new_sgl_scale_ln <- function() {
  new_sgl_scale(class = "sgl_scale_ln")
}

#' @export
sgl_func_name.sgl_scale_ln <- function(sgl_r_obj) {
  "ln"
}

#' @export
valid_scale.sgl_scale_ln <- function(scale, aes, layers, dfs) {
  NextMethod()
  raise_for_non_nums(scale, aes, layers, dfs)
  raise_for_non_pos(scale, aes, layers, dfs)
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
