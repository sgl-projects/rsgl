group_by_col_names <- function(col_expr, aes_mappings, scales) {
  column <- col_expr$column
  if (!col_expr_has_cta(col_expr, "bin")) {
    return(column)
  }
  if (!(list(col_expr) %in% aes_mappings)) {
    transformed_col_name <- sprintf(
      "rsgl.%s.bin.%s",
      scale_name(new_sgl_scale_linear()),
      column
    )
    return(transformed_col_name)
  }
  corresponding_mappings <- aes_mappings[
    aes_mappings %in% list(col_expr)
  ]
  transformed_col_names <- c()
  corresponding_aes <- names(corresponding_mappings)
  corresponding_scales <- scales[corresponding_aes]
  if (length(corresponding_scales) > 0) {
    transformed_col_name <- sprintf(
      "rsgl.%s.bin.%s",
      scale_name(new_sgl_scale_log()),
      column
    )
    transformed_col_names <- c(transformed_col_names, transformed_col_name)
  }
  unscaled_aes <- corresponding_aes[
    !(corresponding_aes %in% names(corresponding_scales))
  ]
  if (length(unscaled_aes) > 0) {
    transformed_col_name <- sprintf(
      "rsgl.%s.bin.%s",
      scale_name(new_sgl_scale_linear()),
      column
    )
    transformed_col_names <- c(transformed_col_names, transformed_col_name)
  }
  transformed_col_names
}

perform_as_for_layer <- function(layer, df, scales) {
  aes_mappings <- layer$aes_mappings
  count_mappings <- filter_col_exprs_by_cta(aes_mappings, "count")
  if (length(count_mappings) == 0) {
    return(df)
  }
  if (!("groupings" %in% names(layer))) {
    aggregated_df <- data.frame(
      rsgl.count = dim(df)[1]
    )
    return(aggregated_df)
  }
  group_by_col_list <- lapply(
    layer$groupings,
    function(col_expr) group_by_col_names(col_expr, aes_mappings, scales)
  )
  group_by_cols <- unlist(group_by_col_list)

  df |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(group_by_cols))) |>
    dplyr::summarize(rsgl.count = dplyr::n())
}
