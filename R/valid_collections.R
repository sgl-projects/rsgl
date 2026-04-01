valid_collections <- function(layer) {
  if (!("collections" %in% names(layer))) {
    return()
  }

  aes_mappings <- layer$aes_mappings
  geom <- layer$geom_expr$geom
  collections <- layer$collections

  if (!is_collective(geom)) {
    unformatted_msg <- paste(
      "Error: collect by clause should not be provided",
      "for non-collective geom %s."
    )
    errmsg <- sprintf(unformatted_msg, geom_name(geom))
    stop(errmsg)
  }

  non_pos_mappings <- aes_mappings[
    names(aes_mappings) %in% .non_pos_aes
  ]
  if (!all(non_pos_mappings %in% collections)) {
    errmsg <- paste(
      "Error: all expressions mapped to from non-positional",
      "aesthetics must be included in the collect by clause."
    )
    stop(errmsg)
  }

  pos_mappings <- aes_mappings[
    names(aes_mappings) %in% .pos_aes
  ]
  uncollected_pos <- pos_mappings[
    !(pos_mappings %in% collections)
  ]
  if (length(uncollected_pos) > extension(geom)) {
    unformatted_msg <- paste(
      "Error: the number of uncollected positional aesthetic",
      "expressions exceeds the extensionality of the %s geom.",
      "Add an expression mapped to from a positional aesthetic",
      "to the collect by clause."
    )
    errmsg <- sprintf(unformatted_msg, geom_name(geom))
    stop(errmsg)
  }
}
