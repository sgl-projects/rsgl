column_exists <- function(col_exprs, df) {
  refs <- unique(
    purrr::map_chr(col_exprs, "column")
  )
  results <- refs %in% names(df)
  names(results) <- refs
  if ("*" %in% names(results)) {
    results["*"] <- TRUE
  }
  results
}

raise_if_col_missing <- function(col_exprs, df) {
  exists_results <- column_exists(col_exprs, df)
  if (!all(exists_results)) {
    unformatted_msg <- "Error: referenced column '%s' not found"
    missing_col_index <- which(!exists_results)[1]
    missing_col_name <- names(exists_results)[missing_col_index]
    errmsg <- sprintf(unformatted_msg, missing_col_name)
    stop(errmsg)
  }
}

valid_column_refs <- function(layer, df) {
  all_col_exprs <- list(
    layer$aes_mappings,
    layer$groupings,
    layer$collections
  )
  lapply(
    all_col_exprs,
    function(col_exprs) raise_if_col_missing(col_exprs, df)
  )
}
