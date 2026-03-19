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
valid_aesthetics.sgl_geom_line <- function(geom, layer, df) {
  valid_positional_aes(layer)
  if ("size" %in% names(layer$aes_mappings)) {
    stop("Error: size is not a valid aesthetic for the line geom.")
  }
}

#' @export
valid_collections.sgl_geom_line <- function(geom, layer, df) {
  if (!("collections" %in% names(layer))) {
    return()
  }
  if (!("groupings" %in% names(layer))) {
    collections <- layer$collections
    identity_collections <- filter_col_exprs_by_cta(collections, "identity")
    non_identity_collections <- collections[
      !(collections %in% identity_collections)
    ]
    if (length(non_identity_collections) > 0) {
      errmsg <- paste(
        "Error: if no groupings are present then all collection",
        "expressions must be untransformed columns."
      )
      stop(errmsg)
    }
  } else {
    aes_mappings <- layer$aes_mappings
    pos_mappings <- aes_mappings[
      names(aes_mappings) %in% .pos_aes
    ]
    groupings <- layer$groupings
    positional_groupings <- groupings[
      groupings %in% pos_mappings
    ]
    collections <- layer$collections
    pos_grps_in_collection <- positional_groupings[
      positional_groupings %in% collections
    ]
    if (length(pos_grps_in_collection) > 0) {
      errmsg <- "Error: cannot collect on positional groupings."
      stop(errmsg)
    }
    non_positional_groupings <- groupings[
      !(groupings %in% pos_mappings)
    ]
    missing_from_collections <- non_positional_groupings[
      !(non_positional_groupings %in% collections)
    ]
    if (length(missing_from_collections) > 0) {
      missing_from_collection <- missing_from_collections[[1]]
      unformatted_msg <- paste(
        "Error: all non-positional groupings must",
        "be included in collect by clause. '%s' found",
        "in grouping but not in collect by clause."
      )
      errmsg <- sprintf(unformatted_msg, missing_from_collection$column)
      stop(errmsg)
    }
    collections_without_grouping <- collections[
      !(collections %in% groupings)
    ]
    if (length(collections_without_grouping) > 0) {
      collection_without_grouping <- collections_without_grouping[[1]]
      unformatted_msg <- paste(
        "Error: when grouping is present, cannot collect by column that",
        "doesn't have corresponding grouping, found '%s'."
      )
      errmsg <- sprintf(unformatted_msg, collection_without_grouping$column)
      stop(errmsg)
    }
  }
}

#' @export
valid_qualifier.sgl_geom_line <- function(geom, layer, df) {
  qualifier <- layer$geom_expr$qual
  potentially_valid_qualifiers <- c("default", "regression")
  if (!(qualifier %in% potentially_valid_qualifiers)) {
    unformatted_errmsg <-
      "Error: the %s qualifier is not supported for the %s geom."
    errmsg <- sprintf(
      unformatted_errmsg,
      qualifier,
      geom_name(layer$geom_expr$geom)
    )
    stop(errmsg)
  }
  if (qualifier == "regression") {
    aes_mappings <- layer$aes_mappings
    identity_mappings <- filter_col_exprs_by_cta(aes_mappings, "identity")
    if (length(identity_mappings) < length(aes_mappings)) {
      errmsg <- paste(
        "Error: column-level transformations and aggregations",
        "are not allowed with the regression qualifier"
      )
      stop(errmsg)
    }
    if ("color" %in% names(aes_mappings)) {
      if (
        is_numerical_mapping(layer, df, "color") ||
          is_temporal_mapping(layer, df, "color")
      ) {
        errmsg <- paste(
          "Error: numerical and temporal color mappings are",
          "not allowed with the regression qualifier"
        )
        stop(errmsg)
      }
    }
  }
}
