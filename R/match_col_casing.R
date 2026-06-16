col_has_match_in_df <- function(col_expr, df) {
  tolower(col_expr$column) %in% tolower(names(df))
}

facet_has_match_in_df <- function(facet_expr, df) {
  tolower(facet_expr$column) %in% tolower(names(df))
}

match_col_to_df <- function(col_expr, df) {
  if (col_has_match_in_df(col_expr, df)) {
    df_names <- names(df)
    match_name <- df_names[
      tolower(col_expr$column) == tolower(df_names)
    ][1]
    col_expr$column <- match_name
  }
  col_expr
}

match_facet_to_df <- function(facet_expr, df) {
  df_names <- names(df)
  match_name <- df_names[
    tolower(facet_expr$column) == tolower(df_names)
  ][1]
  facet_expr$column <- match_name
  facet_expr
}

match_cols_to_df <- function(col_exprs, df) {
  purrr::map(col_exprs, ~ match_col_to_df(., df))
}

match_col_casing_for_layer <- function(layer, df) {
  layer$aes_mappings <- match_cols_to_df(layer$aes_mappings, df)
  if ("groupings" %in% names(layer)) {
    layer$groupings <- match_cols_to_df(layer$groupings, df)
  }
  if ("collections" %in% names(layer)) {
    layer$collections <- match_cols_to_df(layer$collections, df)
  }
  layer
}

match_col_casing_for_facet <- function(facet_expr, dfs) {
  for (df in dfs) {
    if (facet_has_match_in_df(facet_expr, df)) {
      facet_expr <- match_facet_to_df(facet_expr, df)
      return(facet_expr)
    }
  }
  facet_expr
}

match_col_casing_for_layers <- function(layers, dfs) {
  purrr::map2(layers, dfs, match_col_casing_for_layer)
}

match_col_casing_for_facets <- function(facets, dfs) {
  purrr::map(facets, ~ match_col_casing_for_facet(., dfs))
}

match_col_casing <- function(rgs, dfs) {
  rgs$layers <- match_col_casing_for_layers(rgs$layers, dfs)
  if ("facets" %in% names(rgs)) {
    rgs$facets <- match_col_casing_for_facets(rgs$facets, dfs)
  }
  rgs
}
