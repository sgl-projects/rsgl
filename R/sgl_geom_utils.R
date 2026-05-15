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

group_aes_expr <- function(group_cols) {
  if (length(group_cols) == 1) {
    return(as.symbol(group_cols))
  }
  comma_sep_cols <- paste(group_cols, collapse = ", ")
  interaction_str <- paste(
    c("interaction(", comma_sep_cols, ")"),
    collapse = ""
  )
  parse(text = interaction_str)[[1]]
}
