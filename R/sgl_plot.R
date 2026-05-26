#' @export
print.sgl_plot <- function(x, ...) {
  rgs <- x[names(x) != "result_dfs"]
  dfs <- x$result_dfs
  casted_dfs <- cast_columns(rgs, dfs)
  post_cta_dfs <- perform_ctas(rgs, casted_dfs)
  gg_p <- rgs_to_ggplot2(rgs, post_cta_dfs)
  suppressMessages(suppressWarnings(print(gg_p)))
  invisible(x)
}
