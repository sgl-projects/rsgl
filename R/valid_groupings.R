valid_groupings <- function(layer, df) {
  groupings_found <- "groupings" %in% names(layer)

  if (groupings_found) {
    counted_groupings <- filter_col_exprs_by_cta(layer$groupings, "count")
    if (length(counted_groupings) > 0) {
      stop("Error: expressions in the group by clause cannot be aggregations.")
    }
  }

  aes_mappings <- layer$aes_mappings
  count_mappings <- filter_col_exprs_by_cta(aes_mappings, "count")
  aggregations_found <- length(count_mappings) > 0

  if (!aggregations_found && !groupings_found) {
    return()
  }

  non_aggregated_mappings <- aes_mappings[
    !(aes_mappings %in% count_mappings)
  ]

  if (aggregations_found && !groupings_found) {
    if (length(non_aggregated_mappings) > 0) {
      errmsg <- paste(
        "Error: if aggregations are present, then all unaggregated",
        "expressions in the visualize clause must be included",
        "in the group by clause."
      )
      stop(errmsg)
    }
    return()
  }

  if (!aggregations_found && groupings_found) {
    errmsg <- paste(
      "Error: if group by clause is provided then aggregation(s)",
      "must be present in the visualize clause."
    )
    stop(errmsg)
    return()
  }

  non_agg_mapping_has_grouping <- non_aggregated_mappings %in% layer$groupings
  if (!all(non_agg_mapping_has_grouping)) {
    errmsg <- paste(
      "Error: if aggregations are present, then all unaggregated",
      "expressions in the visualize clause must be included",
      "in the group by clause."
    )
    stop(errmsg)
  }
}
