in_at_least_one_data_source <- function(col_name, dfs) {
  in_data_source <- purrr::map_lgl(
    dfs, ~ col_name %in% colnames(.)
  )
  if (any(in_data_source)) {
    return(TRUE)
  }
  FALSE
}

col_type <- function(col_name, df) {
  if (!(col_name %in% names(df))) {
    return(character(0))
  }
  col <- df[[col_name]]
  if (is_categorical_col(col)) {
    return("categorical")
  }
  if (is_numerical_col(col)) {
    return("numerical")
  }
  if (is_temporal_col(col)) {
    return("temporal")
  }
  "unknown"
}
all_types <- function(col_name, dfs) {
  unique(unlist(sapply(dfs, function(df) col_type(col_name, df))))
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
  for (facet_col in facet_col_names) {
    facet_col_types <- all_types(facet_col, dfs)
    if ("unknown" %in% facet_col_types) {
      unformatted_msg <- paste(
        "Error: unknown SGL type classification",
        "(numerical, categorical, or temporal)",
        "for column '%s'."
      )
      err_msg <- sprintf(
        unformatted_msg, facet_col
      )
      stop(err_msg)
    }
    if (length(facet_col_types) > 1) {
      unformatted_msg <- paste(
        "Error: facet column '%s' does not have a consistent type",
        "(categorical, numerical, or temporal) across",
        "all layers where it is present."
      )
      errmsg <- sprintf(unformatted_msg, facet_col)
      stop(errmsg)
    }
  }
}
