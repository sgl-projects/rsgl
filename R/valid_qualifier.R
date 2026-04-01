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
}
