is_cat_or_bin_mapping <- function(layer, df, aes) {
  if (
    is_categorical_mapping(layer, df, aes) ||
      is_binned_mapping(layer, aes)
  ) {
    TRUE
  } else {
    FALSE
  }
}
