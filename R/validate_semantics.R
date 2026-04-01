validate_semantics <- function(rgs, dfs) {
  Map(
    function(layer, df) {
      valid_column_refs(layer, df)
      valid_ctas(layer, df)
      valid_aesthetics(layer)
      valid_qualifier(layer)
      valid_groupings(layer)
      valid_collections(layer)
    },
    rgs$layers,
    dfs
  )
  valid_layering(rgs, dfs)
  valid_scales(rgs, dfs)
  valid_facet(rgs, dfs)
  valid_titles(rgs)
}
