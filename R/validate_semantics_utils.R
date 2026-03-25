column_from_aes <- function(layer, df, aes) {
  col_name_from_aes <- layer$aes_mappings[[aes]]$column
  df[[col_name_from_aes]]
}

counts_are_applied_to_star <- function(layer) {
  aes_mappings <- layer$aes_mapping
  mappings_with_count <- filter_col_exprs_by_cta(aes_mappings, "count")
  counted_cols <- purrr::map_chr(mappings_with_count, "column")
  if (!setequal(counted_cols, "*")) {
    stop("Error: count transformations must be applied to *.")
  }
}
