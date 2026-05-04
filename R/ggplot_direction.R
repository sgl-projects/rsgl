dir_priority_ranking <- function(layer, df, aes) {
  if (is_temporal_mapping(layer, df, aes)) {
    return(1)
  }
  if (is_categorical_mapping(layer, df, aes)) {
    return(2)
  }
  if (is_binned_mapping(layer, aes)) {
    return(3)
  }
  4
}

ggplot_direction <- function(layer, df) {
  geom <- layer$geom_expr$geom
  qual <- layer$geom_expr$qual

  if (qual %in% c("vertical", "horizontal")) {
    return(ggplot_dir_from_qual(geom, qual))
  }

  aes_mappings <- layer$aes_mappings

  pos_mappings <- aes_mappings[
    names(aes_mappings) %in% .pos_aes
  ]
  if (length(pos_mappings) == 1) {
    pos_aes <- names(pos_mappings)
    if (pos_aes %in% c("x", "theta")) {
      return("y")
    }
    return("x")
  }

  if (identical(geom, new_sgl_geom_box())) {
    if ("collections" %in% names(layer)) {
      collections <- layer$collections
      uncollected_pos <- pos_mappings[
        !(pos_mappings %in% collections)
      ]
      if (length(uncollected_pos) == 1) {
        uncollected_aes <- names(uncollected_pos)
        if (uncollected_aes %in% c("x", "theta")) {
          return("y")
        }
        return("x")
      }
    }
  }

  if (names(pos_mappings)[1] %in% .cart_aes) {
    x_rank <- dir_priority_ranking(layer, df, "x")
    y_rank <- dir_priority_ranking(layer, df, "y")
  } else {
    x_rank <- dir_priority_ranking(layer, df, "theta")
    y_rank <- dir_priority_ranking(layer, df, "r")
  }
  if (x_rank <= y_rank) {
    return("x")
  }
  "y"
}
