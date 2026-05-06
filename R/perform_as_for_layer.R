add_scaled_cols <- function(layer, scales, df) {
  if (is.null(scales)) {
    return(df)
  }
  non_lin_scales <- scales[
    purrr::map_lgl(scales, ~ (!identical(., new_sgl_scale_linear())))
  ]
  if (length(non_lin_scales) == 0) {
    return(df)
  }
  non_lin_scaled_aes <- names(non_lin_scales)
  aes_mappings <- layer$aes_mappings
  non_lin_mappings <- aes_mappings[
    names(aes_mappings) %in% non_lin_scaled_aes
  ]
  avg_mappings <- filter_col_exprs_by_cta(non_lin_mappings, "avg")
  if (length(avg_mappings) == 0) {
    return(df)
  }
  for (aes in names(avg_mappings)) {
    scale <- scales[[aes]]
    existing_col <- avg_mappings[[aes]]$column
    new_col <- sprintf(
      "rsgl.%s.%s",
      scale_name(scale),
      existing_col
    )
    if (!(new_col %in% names(df))) {
      df <- df |>
        dplyr::mutate("{new_col}" := apply_scale(scale, .data[[existing_col]]))
    }
  }
  df
}

summarize_args <- function(layer, scales) {
  aes_mappings <- layer$aes_mappings
  viz_aggs <- filter_agg_exprs(aes_mappings)
  args <- list()
  for (aes in names(viz_aggs)) {
    if (aes %in% names(scales)) {
      scale <- scales[[aes]]
    } else {
      scale <- NULL
    }
    col_expr <- viz_aggs[[aes]]
    agg_col_name <- agg_col_name(col_expr$cta, col_expr, scale)
    agg_expr <- agg_col_expr(col_expr$cta, col_expr, scale)
    if (!(agg_col_name %in% names(args))) {
      args[[agg_col_name]] <- agg_expr
    }
  }

  collections <- layer$collections
  addnl_collections <- collections[
    !(collections %in% aes_mappings)
  ]
  collect_aggs <- filter_agg_exprs(addnl_collections)
  for (col_expr in collect_aggs) {
    agg_col_name <- agg_col_name(col_expr$cta, col_expr, NULL)
    agg_expr <- agg_col_expr(col_expr$cta, col_expr, NULL)
    if (!(agg_col_name %in% names(args))) {
      args[[agg_col_name]] <- agg_expr
    }
  }
  args
}

group_by_col_names <- function(col_expr, aes_mappings, scales) {
  column <- col_expr$column
  if (!col_expr_has_cta(col_expr, "bin")) {
    return(column)
  }
  if ("arg" %in% names(col_expr)) {
    num_bins <- col_expr$arg
  } else {
    num_bins <- 30
  }
  if (!(list(col_expr) %in% aes_mappings)) {
    transformed_col_name <- sprintf(
      "rsgl.%s.bin.%s.%s",
      scale_name(new_sgl_scale_linear()),
      num_bins,
      column
    )
    return(transformed_col_name)
  }
  corresponding_mappings <- aes_mappings[
    aes_mappings %in% list(col_expr)
  ]
  transformed_col_names <- c()
  corresponding_aes <- names(corresponding_mappings)
  corresponding_scales <- scales[
    names(scales) %in% corresponding_aes
  ]
  for (scale in corresponding_scales) {
    transformed_col_name <- sprintf(
      "rsgl.%s.bin.%s.%s",
      scale_name(scale),
      num_bins,
      column
    )
    transformed_col_names <- c(transformed_col_names, transformed_col_name)
  }
  unscaled_aes <- corresponding_aes[
    !(corresponding_aes %in% names(corresponding_scales))
  ]
  if (length(unscaled_aes) > 0) {
    transformed_col_name <- sprintf(
      "rsgl.%s.bin.%s.%s",
      scale_name(new_sgl_scale_linear()),
      num_bins,
      column
    )
    transformed_col_names <- c(transformed_col_names, transformed_col_name)
  }
  transformed_col_names
}

backscale_cols <- function(layer, scales, df) {
  if (is.null(scales)) {
    return(df)
  }
  non_lin_scales <- scales[
    purrr::map_lgl(scales, ~ (!identical(., new_sgl_scale_linear())))
  ]
  if (length(non_lin_scales) == 0) {
    return(df)
  }
  non_lin_scaled_aes <- names(non_lin_scales)
  aes_mappings <- layer$aes_mappings
  non_lin_mappings <- aes_mappings[
    names(aes_mappings) %in% non_lin_scaled_aes
  ]
  avg_mappings <- filter_col_exprs_by_cta(non_lin_mappings, "avg")
  if (length(avg_mappings) == 0) {
    return(df)
  }
  for (aes in names(avg_mappings)) {
    scale <- scales[[aes]]
    orig_col_name <- avg_mappings[[aes]]$column
    agg_col_name <- sprintf(
      "rsgl.%s.avg.%s",
      scale_name(scale),
      orig_col_name
    )
    df <- df |>
      dplyr::mutate(
        !!agg_col_name := apply_scale_inverse(scale, .data[[agg_col_name]])
      )
  }
  df
}

perform_as_for_layer <- function(layer, df, scales) {
  aes_mappings <- layer$aes_mappings
  aes_aggs <- filter_agg_exprs(aes_mappings)
  collect_aggs <- filter_agg_exprs(layer$collections)
  all_aggs <- c(aes_aggs, collect_aggs)
  if (length(all_aggs) == 0) {
    return(df)
  }
  scaled_df <- add_scaled_cols(layer, scales, df)
  args <- summarize_args(layer, scales)
  if (!("groupings" %in% names(layer))) {
    aggregated_df <- scaled_df |>
      dplyr::summarize(!!!args)
    return(aggregated_df)
  }
  group_by_col_list <- lapply(
    layer$groupings,
    function(col_expr) group_by_col_names(col_expr, aes_mappings, scales)
  )
  group_by_cols <- unlist(group_by_col_list)

  agg_df <- scaled_df |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(group_by_cols))) |>
    dplyr::summarize(!!!args)

  backscale_cols(layer, scales, agg_df)
}
