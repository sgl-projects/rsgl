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
ggplot_aes.sgl_geom_box <- function(geom, layer, df, scales) {
  result_aes <- NextMethod()
  if (!("group" %in% names(result_aes))) {
    group_cols <- character(0)
    aes_mappings <- layer$aes_mappings
    pos_mappings <- aes_mappings[
      names(aes_mappings) %in% .pos_aes
    ]
    binned_or_cat_bool <- sapply(
      names(pos_mappings),
      function(aes) is_cat_or_bin_mapping(layer, df, aes)
    )
    binned_or_cat_mapping <- pos_mappings[binned_or_cat_bool]
    if (length(binned_or_cat_mapping) > 0) {
      col_name <- mapping_col_name(
        names(binned_or_cat_mapping),
        binned_or_cat_mapping[[1]],
        scales
      )
      group_cols <- c(group_cols, col_name)
    }
    if ("color" %in% names(aes_mappings)) {
      col_name <- mapping_col_name(
        "color",
        aes_mappings$color,
        scales
      )
      group_cols <- c(group_cols, col_name)
    }
    group_cols <- unique(group_cols)
    if (length(group_cols) > 0) {
      if (length(group_cols) == 1) {
        result_aes[["group"]] <- as.symbol(group_cols)
      } else {
        comma_sep_cols <- paste(group_cols, collapse = ", ")
        interaction_str <- paste(
          c("interaction(", comma_sep_cols, ")"),
          collapse = ""
        )
        interaction_expr <- parse(text = interaction_str)[[1]]
        result_aes[["group"]] <- interaction_expr
      }
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
