add_trans_cols_from_aes <- function(aes_mappings, df, scales) {
  for (aes in names(aes_mappings)) {
    aes_mapping <- aes_mappings[[aes]]
    cta <- aes_mapping$cta
    if (is_transformation(cta)) {
      if (aes %in% names(scales)) {
        scale <- scales[[aes]]
      } else {
        scale <- new_sgl_scale_linear()
      }
      df <- add_transformed_column(cta, aes_mapping$column, df, scale = scale)
    }
  }
  df
}

add_trans_cols_from_col_exprs <- function(col_exprs, df) {
  for (col_expr in col_exprs) {
    cta <- col_expr$cta
    if (is_transformation(cta)) {
      df <- add_transformed_column(
        cta, col_expr$column, df,
        scale = new_sgl_scale_linear()
      )
    }
  }
  df
}

perform_cts_for_layer <- function(layer, df, scales) {
  df <- add_trans_cols_from_aes(layer$aes_mappings, df, scales)
  df <- add_trans_cols_from_col_exprs(layer$groupings, df)
  df <- add_trans_cols_from_col_exprs(layer$collections, df)
  df
}
