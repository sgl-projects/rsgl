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
group_aes_cols.sgl_geom_line <- function(geom, layer, df, scales) {
  aes_mappings <- layer$aes_mappings
  if ("color" %in% names(aes_mappings)) {
    mapping_col_name("color", aes_mappings[["color"]], scales)
  } else {
    character(0)
  }
}

#' @export
ggplot_aes.sgl_geom_line <- function(geom, layer, df, scales) {
  ggplot_aes <- NextMethod()
  if (!("group" %in% names(ggplot_aes))) {
    group_cols <- group_aes_cols(geom, layer, df, scales)
    if (length(group_cols) == 0) {
      ggplot_aes$group <- "1"
    } else {
      ggplot_aes$group <- group_aes_expr(group_cols)
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
  c(
    "horizontal",
    "regression",
    "vertical"
  )
}

#' @export
has_direction.sgl_geom_line <- function(geom) {
  TRUE
}

#' @export
ggplot_dir_from_qual.sgl_geom_line <- function(geom, qual) {
  if (qual == "horizontal") {
    return("x")
  }
  "y"
}
