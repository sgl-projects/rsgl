valid_groupings <- function(layer) {
  viz_collect_exprs <- c(layer$aes_mappings, layer$collections)
  viz_collect_aggs <- filter_agg_exprs(viz_collect_exprs)
  viz_collect_unaggs <- viz_collect_exprs[
    !(viz_collect_exprs %in% viz_collect_aggs)
  ]

  if ("groupings" %in% names(layer)) {
    groupings <- layer$groupings

    group_agg_exprs <- filter_agg_exprs(groupings)
    if (length(group_agg_exprs) > 0) {
      stop("Error: group by clause cannot contain aggregation expressions.")
    }

    ungrouped_aggs <- viz_collect_unaggs[
      !(viz_collect_unaggs %in% groupings)
    ]
    if (length(ungrouped_aggs) > 0) {
      errmsg <- paste(
        "Error: unaggregated expressions in the visualize",
        "and collect by clauses must also be present in the",
        "group by clause."
      )
      stop(errmsg)
    }
  } else {
    if (length(viz_collect_aggs) > 0) {
      if (length(viz_collect_unaggs) > 0) {
        errmsg <- paste(
          "Error: given that aggregations are present,",
          "all unaggregated expressions in the visualize",
          "and collect by clause must also be included",
          "in the group by clause.",
          "However, no group by clause was provided."
        )
        stop(errmsg)
      }
    }
  }
}
