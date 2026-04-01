new_sgl_geom_line <- function() {
  new_sgl_geom(class = "sgl_geom_line")
}

#' @export
geom_name.sgl_geom_line <- function(geom) {
  "line"
}

#' @export
is_collective.sgl_geom_line <- function(geom) {
  TRUE
}

#' @export
ggplot_geom.sgl_geom_line <- function(geom) {
  ggplot2::geom_line
}

#' @export
ggplot_aes.sgl_geom_line <- function(geom, layer, df, scales) {
  ggplot_aes <- NextMethod()
  if (!("group" %in% names(ggplot_aes))) {
    if ("colour" %in% names(ggplot_aes)) {
      if (is_cat_or_bin_mapping(layer, df, "color")) {
        ggplot_aes$group <- ggplot_aes$colour
      } else {
        ggplot_aes$group <- "1"
      }
    } else {
      ggplot_aes$group <- "1"
    }
  }
  ggplot_aes
}

#' @export
valid_non_pos_aes.sgl_geom_line <- function(geom) {
  "color"
}

#' @export
extension.sgl_geom_line <- function(geom) {
  2
}

#' @export
valid_qual_list.sgl_geom_line <- function(geom) {
  "regression"
}
