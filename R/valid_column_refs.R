all_column_refs <- function(layer) {
  all_col_exprs <- c(layer$aes_mappings, layer$groupings, layer$collections)
  all_refs <- purrr::map_chr(all_col_exprs, ~ (.$column))
  unique(all_refs)
}

column_exists <- function(refs, df) {
  results <- refs %in% names(df)
  names(results) <- refs
  if ("*" %in% names(results)) {
    results["*"] <- TRUE
  }
  results
}

raise_if_col_missing <- function(exists_results) {
  missing_col_names <- names(exists_results)[!exists_results]
  if (length(missing_col_names) > 0) {
    errmsg <- sprintf(
      "Error: referenced column '%s' not found",
      missing_col_names[1]
    )
    stop(errmsg)
  }
}

valid_column_refs <- function(layer, df) {
  refs <- all_column_refs(layer)
  exists_results <- column_exists(refs, df)
  raise_if_col_missing(exists_results)
}
