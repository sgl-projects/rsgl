in_at_least_one_data_source <- function(col_name, dfs) {
  for (df in dfs) {
    if (col_name %in% colnames(df)) {
      return(TRUE)
    }
  }
  FALSE
}

categorical_in_all_layers <- function(col_name, dfs) {
  for (df in dfs) {
    if (col_name %in% colnames(df)) {
      if (!is_categorical_col(df[[col_name]])) {
        return(FALSE)
      }
    }
  }
  TRUE
}

valid_facet <- function(rgs, dfs) {
  if (!("facets" %in% names(rgs))) {
    return()
  }
  facets <- rgs$facets
  num_facets <- length(facets)
  if (num_facets > 2) {
    stop("Error: cannot have more than two facets.")
  }
  if (num_facets == 2) {
    facet_directions <- purrr::map_chr(facets, "direction")
    if (
      all(facet_directions == "vertical") ||
        all(facet_directions == "horizontal")
    ) {
      stop(
        "Error: for two facets, one must be horizontal and the other vertical."
      )
    }
  }
  facet_col_names <- purrr::map_chr(facets, "column")
  cols_exist <- sapply(
    facet_col_names,
    function(col_name) in_at_least_one_data_source(col_name, dfs)
  )
  missing_cols <- facet_col_names[!cols_exist]
  if (length(missing_cols) > 0) {
    errmsg <- sprintf(
      "Error: facet column '%s' does not exist in any layer data sources.",
      missing_cols[1]
    )
    stop(errmsg)
  }
  cols_are_categorical <- sapply(
    facet_col_names,
    function(col_name) categorical_in_all_layers(col_name, dfs)
  )
  non_categorical_cols <- facet_col_names[!cols_are_categorical]
  if (length(non_categorical_cols) > 0) {
    unformatted_msg <- paste(
      "Error: facet column '%s' is not categorical in at least one layer.",
      "Facet columns must be categorical type."
    )
    errmsg <- sprintf(unformatted_msg, non_categorical_cols[1])
    stop(errmsg)
  }
}
