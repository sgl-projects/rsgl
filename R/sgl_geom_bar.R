new_sgl_geom_bar <- function() {
  new_sgl_geom(class = "sgl_geom_bar")
}

#' @export
geom_name.sgl_geom_bar <- function(geom) {
  "bar"
}

#' @export
ggplot_geom.sgl_geom_bar <- function(geom) {
  ggplot2::geom_bar
}

#' @export
valid_non_pos_aes.sgl_geom_bar <- function(geom) {
  "color"
}

#' @export
ggplot_aes.sgl_geom_bar <- function(geom, layer, df, scales) {
  ggplot_aes_mapping <- NextMethod()
  if ("colour" %in% names(ggplot_aes_mapping)) {
    names(ggplot_aes_mapping)[names(ggplot_aes_mapping) == "colour"] <- "fill"
  }
  ggplot_aes_mapping
}

#' @export
valid_qualifier.sgl_geom_bar <- function(geom, layer, df) {
  qualifier <- layer$geom_expr$qual
  unformatted_errmsg <-
    "Error: the %s qualifier is not supported for the %s geom"
  valid_qualifiers <- c("default", "unstacked")
  if (!(qualifier %in% valid_qualifiers)) {
    errmsg <- sprintf(
      unformatted_errmsg, qualifier, geom_name(layer$geom_expr$geom)
    )
    stop(errmsg)
  }
}
