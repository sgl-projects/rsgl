validate_semantics <- function(rgs, dfs) {
  Map(
    function(layer, df) {
      valid_column_refs(layer, df)
      valid_ctas(layer, df)
      valid_aesthetics(layer$geom_expr$geom, layer)
      valid_qualifier(layer$geom_expr$geom, layer)
      valid_groupings(layer, df)
      valid_collections(layer$geom_expr$geom, layer, df)
    },
    rgs$layers,
    dfs
  )
  valid_layering(rgs, dfs)
  valid_scales(rgs, dfs)
  valid_facet(rgs, dfs)
  valid_titles(rgs)
}
