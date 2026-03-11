default_title <- function(col_expr) {
  if (col_expr_has_cta(col_expr, "identity")) {
    return(col_expr$column)
  }
  if (col_expr_has_cta(col_expr, "count")) {
    return("Count")
  }
  paste("Binned", col_expr$column)
}

title_for_aes <- function(aes, rgs) {
  titles <- rgs$titles
  if (aes %in% names(titles)) {
    return(titles[[aes]])
  }
  # this function does not distinguish between fill and colour
  # for bars and non-geoms, it treats all geoms equally
  # and gets the default title from the first layer
  # where a color mapping is present
  for (layer in rev(rgs$layers)) {
    if (aes %in% names(layer$aes_mappings)) {
      return(default_title(layer$aes_mappings[[aes]]))
    }
  }
}

ggplot_labs <- function(rgs) {
  all_aes <- all_aesthetics(rgs)
  non_color_aes <- setdiff(all_aes, "color")
  labs_args <- lapply(
    non_color_aes,
    function(aes) title_for_aes(aes, rgs)
  )
  names(labs_args) <- non_color_aes
  names(labs_args)[names(labs_args) == "theta"] <- "x"
  names(labs_args)[names(labs_args) == "r"] <- "y"
  if ("color" %in% all_aes) {
    bar_layers <- list()
    non_bar_layers <- list()
    for (layer in rgs$layers) {
      if ("color" %in% names(layer$aes_mappings)) {
        if (identical(layer$geom_expr$geom, new_sgl_geom_bar())) {
          bar_layers <- c(bar_layers, list(layer))
        } else {
          non_bar_layers <- c(non_bar_layers, list(layer))
        }
      }
    }
    if (length(bar_layers) > 0) {
      bar_filtered_rgs <- rgs
      bar_filtered_rgs$layers <- bar_layers
      fill_lab <- title_for_aes("color", bar_filtered_rgs)
      labs_args$fill <- fill_lab
    }
    if (length(non_bar_layers) > 0) {
      non_bar_filtered_rgs <- rgs
      non_bar_filtered_rgs$layers <- non_bar_layers
      colour_lab <- title_for_aes("color", non_bar_filtered_rgs)
      labs_args$colour <- colour_lab
    }
  }
  do.call(ggplot2::labs, labs_args)
}
