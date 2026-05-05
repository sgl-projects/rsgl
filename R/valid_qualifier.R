expected_box_dir <- function(ext_aes) {
  if (ext_aes %in% c("x", "theta")) {
    return("horizontal")
  }
  "vertical"
}

valid_box_direction <- function(layer) {
  aes_mappings <- layer$aes_mappings
  direction <- layer$geom_expr$qual
  pos_mappings <- aes_mappings[
    names(aes_mappings) %in% .pos_aes
  ]
  if (length(pos_mappings) == 1) {
    pos_aes <- names(pos_mappings)
    expected_dir <- expected_box_dir(pos_aes)
    if (direction != expected_dir) {
      unformatted_msg <- paste(
        "Error: a single positional aesthetic",
        "of %s does not align with the %s qualifier",
        "for the box geom."
      )
      err_msg <- sprintf(
        unformatted_msg, pos_aes, direction
      )
      stop(err_msg)
    }
  } else {
    uncollected_mappings <- pos_mappings[
      !(pos_mappings %in% layer$collections)
    ]
    if (length(uncollected_mappings) == 1) {
      uncollected_aes <- names(uncollected_mappings)
      expected_dir <- expected_box_dir(uncollected_aes)
      if (direction != expected_dir) {
        unformatted_msg <- paste(
          "Error: a single uncollected positional aesthetic",
          "of %s does not align with the %s qualifier",
          "for the box geom."
        )
        err_msg <- sprintf(
          unformatted_msg, uncollected_aes, direction
        )
        stop(err_msg)
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
    err_msg <- sprintf(
      "Error: the %s qualifier is not valid for the %s geom.",
      qual,
      geom_name(geom)
    )
    stop(err_msg)
  }
  if (identical(geom, new_sgl_geom_box())) {
    valid_box_direction(layer)
  }
}
