new_sgl_geom_point <- function() {
  new_sgl_geom(class = "sgl_geom_point")
}

#' @export
geom_name.sgl_geom_point <- function(geom) {
  "point"
}

#' @export
ggplot_geom.sgl_geom_point <- function(geom) {
  ggplot2::geom_point
}

#' @export
valid_qualifier.sgl_geom_point <- function(geom, layer, df) {
  qualifier <- layer$geom_expr$qual
  unformatted_errmsg <- paste(
    "Error: the %s qualifier is not",
    "supported for the %s geom"
  )
  valid_qualifiers <- c("default", "jittered")
  if (!(qualifier %in% valid_qualifiers)) {
    errmsg <- sprintf(
      unformatted_errmsg,
      qualifier,
      geom_name(layer$geom_expr$geom)
    )
    stop(errmsg)
  }
}
