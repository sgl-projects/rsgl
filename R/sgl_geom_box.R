new_sgl_geom_box <- function() {
  new_sgl_geom(class = "sgl_geom_box")
}

#' @export
geom_name.sgl_geom_box <- function(geom) {
  "box"
}

#' @export
is_collective.sgl_geom_box <- function(geom) {
  TRUE
}

#' @export
valid_non_pos_aes.sgl_geom_box <- function(geom) {
  "color"
}

#' @export
extension.sgl_geom_box <- function(geom) {
  1
}

#' @export
ggplot_geom.sgl_geom_box <- function(geom) {
  ggplot2::geom_boxplot
}

#' @export
group_aes_cols.sgl_geom_box <- function(geom, layer, df, scales) {
  group_cols <- character()
  aes_mappings <- layer$aes_mappings
  pos_mappings <- aes_mappings[
    names(aes_mappings) %in% .pos_aes
  ]
  if (length(pos_mappings) == 2) {
    qual <- layer$geom_expr$qual
    if (qual %in% c("horizontal", "vertical")) {
      dir <- layer$geom_expr$qual
      if (names(pos_mappings)[1] %in% .cart_aes) {
        if (dir == "horizontal") {
          aes_for_group <- "y"
        } else {
          aes_for_group <- "x"
        }
      } else {
        if (dir == "horizontal") {
          aes_for_group <- "r"
        } else {
          aes_for_group <- "theta"
        }
      }
    } else {
      ggplot_dir <- ggplot_direction(layer, df)
      if (names(pos_mappings)[1] %in% .cart_aes) {
        if (ggplot_dir == "x") {
          aes_for_group <- "x"
        } else {
          aes_for_group <- "y"
        }
      } else {
        if (ggplot_dir == "x") {
          aes_for_group <- "theta"
        } else {
          aes_for_group <- "r"
        }
      }
    }
    group_col <- mapping_col_name(
      aes_for_group,
      pos_mappings[[aes_for_group]],
      scales
    )
    group_cols <- c(group_cols, group_col)
  }
  if ("color" %in% names(aes_mappings)) {
    group_col <- mapping_col_name("color", aes_mappings[["color"]], scales)
    group_cols <- c(group_cols, group_col)
  }
  unique(group_cols)
}

#' @export
ggplot_aes.sgl_geom_box <- function(geom, layer, df, scales) {
  result_aes <- NextMethod()
  if (!("group" %in% names(result_aes))) {
    group_cols <- group_aes_cols(geom, layer, df, scales)
    if (length(group_cols) > 0) {
      result_aes[["group"]] <- group_aes_expr(group_cols)
    }
  }
  result_aes
}

#' @export
valid_qual_list.sgl_geom_box <- function(geom) {
  c(
    "horizontal",
    "vertical"
  )
}

#' @export
has_direction.sgl_geom_box <- function(geom) {
  TRUE
}

#' @export
ggplot_dir_from_qual.sgl_geom_box <- function(geom, qual) {
  if (qual == "vertical") {
    return("x")
  }
  "y"
}
