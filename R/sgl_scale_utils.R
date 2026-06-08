non_numerical_in_layer <- function(aes, layer, df) {
  if (aes %in% names(layer$aes_mappings)) {
    if (
      is_categorical_mapping(layer, df, aes) ||
        is_temporal_mapping(layer, df, aes)
    ) {
      return(TRUE)
    }
  }
  FALSE
}

raise_for_non_nums <- function(scale, aes, layers, dfs) {
  non_numericals <- mapply(
    function(layer, df) non_numerical_in_layer(aes, layer, df),
    layers,
    dfs
  )
  if (any(non_numericals)) {
    unformatted_msg <- paste(
      "Error: the %s scale can only be applied",
      "to aesthetics with numerical mappings."
    )
    errmsg <- sprintf(unformatted_msg, sgl_func_name(scale))
    stop(errmsg)
  }
}

non_pos_in_layer <- function(aes, layer, df) {
  aes_mappings <- layer$aes_mappings
  if (aes %in% names(aes_mappings)) {
    col_expr <- aes_mappings[[aes]]
    if (!col_expr_has_cta(col_expr, "count")) {
      col <- column_from_aes(layer, df, aes)
      if (any(col <= 0, na.rm = TRUE)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

raise_for_non_pos <- function(scale, aes, layers, dfs) {
  non_pos_found <- purrr::map2_lgl(
    layers, dfs,
    function(layer, df) non_pos_in_layer(aes, layer, df)
  )
  if (any(non_pos_found)) {
    unformatted_msg <- paste(
      "Error: the %s scale can only be applied to aesthetics",
      "where all values from mappings are positive."
    )
    err_msg <- sprintf(unformatted_msg, sgl_func_name(scale))
    stop(err_msg)
  }
}

ggplot_color_aes <- function(rgs) {
  ggplot_aes <- character(0)
  for (layer in rgs$layers) {
    if ("color" %in% names(layer$aes_mappings)) {
      if (identical(layer$geom_expr$geom, new_sgl_geom_bar())) {
        ggplot_aes <- c(ggplot_aes, "fill")
      } else {
        ggplot_aes <- c(ggplot_aes, "colour")
      }
    }
  }
  ggplot_aes <- unique(ggplot_aes)
  ggplot_aes
}

ggplot_continuous_scales <- function(ggplot_transform, aes, rgs) {
  if (aes %in% c("x", "theta")) {
    scale_fns <- list(quote(ggplot2::scale_x_continuous))
  } else if (aes %in% c("y", "r")) {
    scale_fns <- list(quote(ggplot2::scale_y_continuous))
  } else if (aes == "size") {
    scale_fns <- list(quote(ggplot2::scale_size))
  } else if (aes == "color") {
    ggplot_aes_scale_map <- list(
      colour = quote(ggplot2::scale_colour_continuous),
      fill = quote(ggplot2::scale_fill_continuous)
    )
    named_scale_fns <- ggplot_aes_scale_map[
      ggplot_color_aes(rgs)
    ]
    scale_fns <- unname(named_scale_fns)
  }

  fn_args <- list(transform = ggplot_transform)
  ggplot_scales <- lapply(
    scale_fns,
    function(scale_fn) eval(as.call(c(scale_fn, fn_args)))
  )
  ggplot_scales
}
