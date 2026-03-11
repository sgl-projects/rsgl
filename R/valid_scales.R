valid_scales <- function(rgs, dfs) {
  if (!("scales" %in% names(rgs))) {
    return()
  }
  scales <- rgs$scales
  for (i in seq_along(scales)) {
    aes <- names(scales)[i]
    scale <- scales[[i]]
    valid_scale(scale, aes, rgs$layers, dfs)
  }
}
