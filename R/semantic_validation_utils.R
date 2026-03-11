column_from_aes <- function(layer, df, aes) {
  col_name_from_aes <- layer$aes_mappings[[aes]]$column
  df[[col_name_from_aes]]
}

valid_positional_aes <- function(layer) {
  actual_aes_names <- names(layer$aes_mappings)
  cart_coords_found <- intersect(.cart_aes, actual_aes_names)
  polar_coords_found <- intersect(.polar_aes, actual_aes_names)

  if (length(cart_coords_found) == 0 && length(polar_coords_found) == 0) {
    stop("Error: positional aesthetics must be provided, none were found.")
  }

  if (length(cart_coords_found) > 0 && length(polar_coords_found) > 0) {
    errmsg <- paste(
      "Error: found aesthetics from multiple coordinate systems.",
      "All positional aesthetics must be from a single coordinate system."
    )
    stop(errmsg)
  }
}

counts_are_applied_to_star <- function(layer) {
  aes_mappings <- layer$aes_mapping
  mappings_with_count <- filter_col_exprs_by_cta(aes_mappings, "count")
  counted_cols <- purrr::map_chr(mappings_with_count, "column")
  if (!setequal(counted_cols, "*")) {
    stop("Error: count transformations must be applied to *.")
  }
}
