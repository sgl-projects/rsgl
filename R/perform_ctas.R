perform_ctas_for_layer <- function(layer, df, scales) {
  post_ct_df <- perform_cts_for_layer(layer, df, scales)
  post_cta_df <- perform_as_for_layer(layer, post_ct_df, scales)
  post_cta_df
}

perform_ctas <- function(rgs, dfs) {
  Map(
    function(layer, df) perform_ctas_for_layer(layer, df, rgs$scales),
    rgs$layers,
    dfs
  )
}
