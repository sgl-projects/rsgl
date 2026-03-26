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
valid_qual_list.sgl_geom_point <- function(geom) {
  "jittered"
}
