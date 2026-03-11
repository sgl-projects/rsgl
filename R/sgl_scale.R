new_sgl_scale <- function(class = character()) {
  structure(
    list(),
    class = c(class, "sgl_scale")
  )
}

#' @export
scale_name.sgl_scale <- function(scale) {
  "base"
}

#' @export
valid_scale.sgl_scale <- function(scale, aes, layers, dfs) {
  mapped_aes_by_layer <- lapply(
    layers,
    function(layer) names(layer$aes_mappings)
  )
  all_mapped_aes <- unique(unlist(mapped_aes_by_layer))
  if (!(aes %in% all_mapped_aes)) {
    stop("Error: a scaled aesthetic must have at least one mapping")
  }
}
