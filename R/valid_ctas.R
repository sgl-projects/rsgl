valid_col_expr_ctas <- function(col_exprs, df) {
  lapply(
    col_exprs,
    function(col_expr) valid_cta(col_expr$cta, col_expr, df)
  )
}

valid_ctas <- function(layer, df) {
  valid_col_expr_ctas(layer$aes_mappings, df)
  valid_col_expr_ctas(layer$groupings, df)
  valid_col_expr_ctas(layer$collections, df)
}
