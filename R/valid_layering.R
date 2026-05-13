valid_layering_for_aes <- function(rgs, dfs, aes) {
  if (aes %in% .pos_aes) {
    aes_in_layer <- sapply(
      rgs$layers,
      function(layer) aes %in% names(layer$aes_mappings)
    )
    if (!all(aes_in_layer)) {
      unformatted_msg <- paste(
        "Error: if a positional aesthetic is present in one layer,",
        "it must be present in all layers. '%s' is not present in all layers."
      )
      errmsg <- sprintf(unformatted_msg, aes)
      stop(errmsg)
    }
  }
  numerical_found <- 0
  categorical_found <- 0
  temporal_found <- 0
  date_found <- 0
  datetime_found <- 0
  type_found_in_layer <- function(layer, df) {
    aes_mappings <- layer$aes_mappings
    if (aes %in% names(aes_mappings)) {
      if (is_numerical_mapping(layer, df, aes)) {
        numerical_found <<- 1
      } else if (is_categorical_mapping(layer, df, aes)) {
        categorical_found <<- 1
      } else if (is_temporal_mapping(layer, df, aes)) {
        temporal_found <<- 1
        col <- column_from_aes(layer, df, aes)
        if (class(col)[1] == "Date") {
          date_found <<- 1
        } else if (class(col)[1] == "POSIXct") {
          datetime_found <<- 1
        }
      }
    }
  }
  Map(type_found_in_layer, rgs$layers, dfs)
  types_vec <- c("numerical", "categorical", "temporal")
  found_vec <- sapply(
    c(numerical_found, categorical_found, temporal_found),
    as.logical
  )
  types_found <- types_vec[found_vec]
  if (length(types_found) > 1) {
    unformatted_msg <- paste(
      "Error: an aesthetic must be mapped to the same type",
      "(numerical, categorical, or temporal) across layers.",
      "Found the following types for the %s aesthetic: %s."
    )
    errmsg <- sprintf(
      unformatted_msg,
      aes,
      paste0(types_found, collapse = ", ")
    )
    stop(errmsg)
  }
  if (temporal_found == 1) {
    if (date_found == 1 && datetime_found == 1) {
      unformatted_msg <- paste(
        "Error: an aesthetic cannot be mapped to both date and datetime",
        "types across layers. Found both for the %s aesthetic."
      )
      errmsg <- sprintf(unformatted_msg, aes)
      stop(errmsg)
    }
  }
}

valid_layering <- function(rgs, dfs) {
  aes_to_validate <- all_aesthetics(rgs)
  for (aes in aes_to_validate) {
    valid_layering_for_aes(rgs, dfs, aes)
  }
}
