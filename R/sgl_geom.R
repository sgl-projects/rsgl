new_sgl_geom <- function(class = character()) {
  structure(
    list(),
    class = c(class, "sgl_geom")
  )
}

#' @export
geom_name.sgl_geom <- function(geom) {
  "geom"
}

#' @export
is_collective.sgl_geom <- function(geom) {
  FALSE
}

#' @export
valid_qual_list.sgl_geom <- function(geom) {
  .all_quals
}

#' @export
valid_qualifier.sgl_geom <- function(geom, layer) {
  qual <- layer$geom_expr$qual
  if (qual == "default") {
    return()
  }
  if (!(qual %in% valid_qual_list(geom))) {
    errmsg <- sprintf(
      "Error: the %s qualifier is not valid for the %s geom.",
      qual,
      geom_name(geom)
    )
    stop(errmsg)
  }
}

mapping_col_name <- function(aes, col_expr, scales) {
  if (col_expr_has_cta(col_expr, "identity")) {
    return(col_expr$column)
  }
  if (col_expr_has_cta(col_expr, "count")) {
    return("rsgl.count")
  }
  if (col_expr_has_cta(col_expr, "bin")) {
    if (aes %in% names(scales)) {
      col_name <- sprintf(
        "rsgl.log.bin.%s",
        col_expr$column
      )
    } else {
      col_name <- sprintf(
        "rsgl.linear.bin.%s",
        col_expr$column
      )
    }
    col_name
  }
}

#' @export
ggplot_aes.sgl_geom <- function(geom, layer, df, scales) {
  aes_mappings <- layer$aes_mappings
  aes_args <- list()
  for (aes in names(aes_mappings)) {
    col_to_map_to <- mapping_col_name(aes, aes_mappings[[aes]], scales)
    aes_args[[aes]] <- as.symbol(col_to_map_to)
  }
  names(aes_args)[names(aes_args) == "theta"] <- "x"
  names(aes_args)[names(aes_args) == "r"] <- "y"

  if (!("x" %in% names(aes_args))) {
    aes_args["x"] <- ""
  }
  if (!("y" %in% names(aes_args))) {
    aes_args["y"] <- ""
  }

  group_cols <- c()
  for (collection in layer$collections) {
    corresponding_aes <- aes_mappings[
      aes_mappings %in% list(collection)
    ]
    if (length(corresponding_aes) > 0) {
      for (aes in names(corresponding_aes)) {
        col_to_map_to <- mapping_col_name(aes, corresponding_aes[[aes]], scales)
        group_cols <- c(group_cols, col_to_map_to)
      }
    } else {
      if (col_expr_has_cta(collection, "identity")) {
        group_cols <- c(group_cols, collection$column)
      } else {
        col_name <- sprintf(
          "rsgl.linear.bin.%s",
          collection$column
        )
        group_cols <- c(group_cols, col_name)
      }
    }
  }
  group_cols <- unique(group_cols)
  if (length(group_cols) > 0) {
    if (length(group_cols) == 1) {
      aes_args[["group"]] <- as.symbol(group_cols)
    } else {
      comma_sep_cols <- paste(group_cols, collapse = ", ")
      interaction_str <- paste(
        c("interaction(", comma_sep_cols, ")"),
        collapse = ""
      )
      interaction_expr <- parse(text = interaction_str)[[1]]
      aes_args[["group"]] <- interaction_expr
    }
  }

  do.call(ggplot2::aes, aes_args)
}
