valid_aesthetics <- function(layer) {
  geom <- layer$geom_expr$geom
  actual_aes_names <- names(layer$aes_mappings)
  cart_coords_found <- intersect(.cart_aes, actual_aes_names)
  polar_coords_found <- intersect(.polar_aes, actual_aes_names)

  if (length(cart_coords_found) == 0 && length(polar_coords_found) == 0) {
    stop("Error: positional mapping(s) must be provided, none were found.")
  }

  if (length(cart_coords_found) > 0 && length(polar_coords_found) > 0) {
    errmsg <- paste(
      "Error: found aesthetics from multiple coordinate systems.",
      "All positional aesthetics must be from a single coordinate system."
    )
    stop(errmsg)
  }

  non_pos_aes_found <- setdiff(actual_aes_names, .pos_aes)
  invalid_non_pos_aes <- setdiff(
    non_pos_aes_found,
    valid_non_pos_aes(geom)
  )
  if (length(invalid_non_pos_aes) > 0) {
    errmsg <- sprintf(
      "Error: the %s aesthetic is not valid for the %s geom.",
      invalid_non_pos_aes[1],
      geom_name(geom)
    )
    stop(errmsg)
  }
}
