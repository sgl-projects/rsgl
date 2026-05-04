valid_box_direction <- function(layer) {
  aes_mappings <- layer$aes_mappings
  direction <- layer$geom_expr$qual
  pos_mappings <- aes_mappings[
    names(aes_mappings) %in% .pos_aes
  ]
  if (length(pos_mappings) == 1) {
    pos_aes <- names(pos_mappings)
    if (pos_aes %in% c("x", "theta")) {
      expected_dir <- "horizontal"
    } else {
      expected_dir <- "vertical"
    }
    if (direction != expected_dir) {
      unformatted_msg <- paste(
        "Error: a single positional aesthetic",
        "of %s does not align with the %s qualifier",
        "for the box geom."
      )
      errmsg <- sprintf(
        unformatted_msg, pos_aes, direction
      )
      stop(errmsg)
    }
  } else {
    uncollected_mappings <- pos_mappings[
      !(pos_mappings %in% layer$collections)
    ]
    if (length(uncollected_mappings) == 1) {
      uncollected_aes <- names(uncollected_mappings)
      if (uncollected_aes %in% c("x", "theta")) {
        expected_dir <- "horizontal"
      } else {
        expected_dir <- "vertical"
      }
      if (direction != expected_dir) {
        unformatted_msg <- paste(
          "Error: a single uncollected positional aesthetic",
          "of %s does not align with the %s qualifier",
          "for the box geom."
        )
        errmsg <- sprintf(
          unformatted_msg, uncollected_aes, direction
        )
        stop(errmsg)
      }
    }
  }
}

valid_qualifier <- function(layer) {
  geom <- layer$geom_expr$geom
  qual <- layer$geom_expr$qual
  if (qual == "default") {
    return()
  }
  if (!(qual %in% valid_qual_list(geom))) {
    errmsg <- sprintf(
      "Error: the %s qualifier is not valid for the %s geom.",
      qual,
      geom_name(geom)
    )
    stop(errmsg)
  }
  if (identical(geom, new_sgl_geom_box())) {
    valid_box_direction(layer)
  }
}
