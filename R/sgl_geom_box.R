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
valid_aesthetics.sgl_geom_box <- function(geom, layer, df) {
  valid_positional_aes(layer)
  aes_mappings <- layer$aes_mappings
  pos_aes_names <- .pos_aes[
    .pos_aes %in% names(aes_mappings)
  ]
  if (length(pos_aes_names) == 1) {
    if (is_cat_or_bin_mapping(layer, df, pos_aes_names)) {
      errmsg <- paste(
        "Error: for a box geom with one positional aesthetic mapping,",
        "the mapping must be to a numerical or temporal (unbinned) type."
      )
      stop(errmsg)
    }
  } else {
    first_is_cat_or_binned <-
      is_cat_or_bin_mapping(layer, df, pos_aes_names[1])
    second_is_cat_or_binned <-
      is_cat_or_bin_mapping(layer, df, pos_aes_names[2])
    both_are_cat_or_binned <- first_is_cat_or_binned && second_is_cat_or_binned
    neither_is_cat_or_binned <-
      !first_is_cat_or_binned && !second_is_cat_or_binned
    if (both_are_cat_or_binned || neither_is_cat_or_binned) {
      errmsg <- paste(
        "Error: for a box geom with two positional aesthetic mappings,",
        "one mapping must be categorical or binned,",
        "and the other must be numerical or temporal (unbinned)."
      )
      stop(errmsg)
    }
  }
  if ("size" %in% names(aes_mappings)) {
    errmsg <- sprintf(
      "Error: the size aesthetic is not valid for the %s geom.",
      geom_name(geom)
    )
    stop(errmsg)
  }
  color <- "color"
  if (color %in% names(aes_mappings)) {
    if (!is_cat_or_bin_mapping(layer, df, color)) {
      unformatted_msg <- paste(
        "Error: for the %s geom, %s can only be mapped",
        "to a categorical or binned column."
      )
      errmsg <- sprintf(
        unformatted_msg,
        geom_name(geom),
        color
      )
      stop(errmsg)
    }
  }
}

#' @export
valid_collections.sgl_geom_box <- function(geom, layer, df) {
  if (!("collections" %in% names(layer))) {
    return()
  }
  collections <- layer$collections
  count_exprs <- filter_col_exprs_by_cta(collections, "count")
  if (length(count_exprs) > 0) {
    stop("Error: cannot collect by a count.")
  }
  aes_mappings <- layer$aes_mappings
  all_aes_names <- names(aes_mappings)
  pos_aes_names <- all_aes_names[
    all_aes_names %in% .pos_aes
  ]
  is_cat_or_binned <- sapply(
    pos_aes_names,
    function(aes) is_cat_or_bin_mapping(layer, df, aes)
  )
  cat_or_binned_pos_names <- pos_aes_names[is_cat_or_binned]
  cat_or_binned_pos_mappings <- aes_mappings[
    names(aes_mappings) %in% cat_or_binned_pos_names
  ]
  if (
    any(
      !(cat_or_binned_pos_mappings %in% collections)
    )
  ) {
    errmsg <- paste(
      "Error: For the box geom, categorical or binned",
      "columns mapped to positional aesthetics must be included",
      "in an explicit collection if it is provided."
    )
    stop(errmsg)
  }
  non_cat_binned_pos_names <- pos_aes_names[!is_cat_or_binned]
  non_cat_binned_pos_mappings <- aes_mappings[
    names(aes_mappings) %in% non_cat_binned_pos_names
  ]
  if (
    any(
      non_cat_binned_pos_mappings %in% collections
    )
  ) {
    errmsg <- paste(
      "Error: For the box geom, unbinned numerical or temporal columns mapped",
      "to positional aesthetics cannot be included in an explicit collection."
    )
    stop(errmsg)
  }
  if ("color" %in% names(aes_mappings)) {
    color_mapping <- aes_mappings["color"]
    if (!(color_mapping %in% collections)) {
      unformatted_msg <- paste(
        "Error: For the %s geom, color mappings must have corresponding",
        "collections if an explicit collection is provided."
      )
      errmsg <- sprintf(
        unformatted_msg,
        geom_name(geom)
      )
      stop(errmsg)
    }
  }
  if ("groupings" %in% names(layer)) {
    if (
      any(
        !(collections %in% layer$groupings)
      )
    ) {
      unformatted_msg <- paste(
        "Error: For the %s geom with a group by clause, cannot collect",
        "on an expression that doesn't have a corresponding grouping."
      )
      errmsg <- sprintf(
        unformatted_msg,
        geom_name(geom)
      )
      stop(errmsg)
    }
  }
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
valid_qualifier.sgl_geom_box <- function(geom, layer, df) {
  if (layer$geom_expr$qual != "default") {
    errmsg <- sprintf(
      "Error: geom qualifiers are not allowed for the %s geom.",
      geom_name(geom)
    )
    stop(errmsg)
  }
}
