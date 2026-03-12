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

is_cat_or_bin_mapping <- function(layer, df, aes) {
  if (is_categorical_mapping(layer, df, aes) || is_binned_mapping(layer, aes)) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
valid_aesthetics.sgl_geom_bar <- function(geom, layer, df) {
  valid_positional_aes(layer)
  all_aes <- names(layer$aes_mappings)
  pos_aes <- intersect(.pos_aes, all_aes)
  if (length(pos_aes) == 1) {
    if (is_cat_or_bin_mapping(layer, df, pos_aes)) {
      errmsg <- paste(
        "Error: if only one positional aesthetic is provided for the bar geom,",
        "then it must be mapped to a numerical or temporal type (unbinned)."
      )
      stop(errmsg)
    }
  } else {
    if (
      is_cat_or_bin_mapping(layer, df, pos_aes[1]) &&
        is_cat_or_bin_mapping(layer, df, pos_aes[2])
    ) {
      unformatted_msg <- paste(
        "Error: one of '%s' or '%s' should be a numerical",
        "or temporal mapping (unbinned) for the bar geom"
      )
      errmsg <- sprintf(unformatted_msg, pos_aes[1], pos_aes[2])
      stop(errmsg)
    }
    if (
      !is_cat_or_bin_mapping(layer, df, pos_aes[1]) &&
        !is_cat_or_bin_mapping(layer, df, pos_aes[2])
    ) {
      unformatted_msg <- paste(
        "Error: one of '%s' or '%s' should be a categorical",
        "or binned mapping for the bar geom"
      )
      errmsg <- sprintf(unformatted_msg, pos_aes[1], pos_aes[2])
      stop(errmsg)
    }
  }
  if ("size" %in% all_aes) {
    stop("Error: size is not a valid aesthetic for the bar geom.")
  }
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
