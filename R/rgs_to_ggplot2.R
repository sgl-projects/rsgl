ggplot_facet <- function(rgs_facets) {
  if (length(rgs_facets) == 1) {
    column <- rgs_facets[[1]]$column
    direction <- rgs_facets[[1]]$direction
    column_symbol <- as.symbol(column)
    column_vars <- do.call(ggplot2::vars, list(column_symbol))
    if (direction %in% c("default", "horizontal")) {
      return(ggplot2::facet_grid(cols = column_vars))
    } else {
      return(ggplot2::facet_grid(rows = column_vars))
    }
  }
  all_directions <- purrr::map_chr(rgs_facets, "direction")
  all_columns <- purrr::map_chr(rgs_facets, "column")
  if (setequal(all_directions, "default")) {
    horizontal_index <- 2
    vertical_index <- 1
  } else if (setequal(all_directions, c("horizontal", "vertical"))) {
    horizontal_index <- which(all_directions == "horizontal")
    vertical_index <- which(all_directions == "vertical")
  } else if ("horizontal" %in% all_directions) {
    horizontal_index <- which(all_directions == "horizontal")
    vertical_index <- setdiff(1:2, horizontal_index)
  } else {
    vertical_index <- which(all_directions == "vertical")
    horizontal_index <- setdiff(1:2, vertical_index)
  }
  cols_symbol <- as.symbol(all_columns[horizontal_index])
  rows_symbol <- as.symbol(all_columns[vertical_index])
  facet_grid_args <- list(
    cols = do.call(ggplot2::vars, list(cols_symbol)),
    rows = do.call(ggplot2::vars, list(rows_symbol))
  )

  facet_grid_result <- do.call(ggplot2::facet_grid, facet_grid_args)
  facet_grid_result
}

ggplot_layer <- function(rgs_layer, df, scales) {
  geom <- rgs_layer$geom_expr$geom
  qual <- rgs_layer$geom_expr$qual

  ggplot_layer_args <- list(
    data = df,
    mapping = ggplot_aes(geom, rgs_layer, df, scales)
  )
  if (qual == "regression") {
    ggplot_layer_args$stat <- "smooth"
    ggplot_layer_args$method <- "lm"
  } else if (!identical(geom, new_sgl_geom_box())) {
    ggplot_layer_args$stat <- "identity"
  }
  if (qual == "jittered") {
    ggplot_layer_args$position <- "jitter"
  } else if (qual == "unstacked") {
    ggplot_layer_args$position <- "identity"
  }

  if (has_direction(geom)) {
    ggplot_layer_args$orientation <- ggplot_direction(rgs_layer, df)
  }

  layer <- do.call(ggplot_geom(geom), ggplot_layer_args)
  layer
}

rgs_to_ggplot2 <- function(rgs, dfs) {
  plot <- ggplot2::ggplot()

  ggplot_layers <- Map(
    function(layer, df) ggplot_layer(layer, df, rgs$scales),
    rgs$layers,
    dfs
  )

  for (layer in ggplot_layers) {
    plot <- plot + layer
  }

  if (ggplot2::as_label(ggplot_layers[[1]]$mapping$x) == "\"\"") {
    plot <- plot + ggplot2::labs(x = NULL)
    plot <- plot + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  }
  if (ggplot2::as_label(ggplot_layers[[1]]$mapping$y) == "\"\"") {
    plot <- plot + ggplot2::labs(y = NULL)
    plot <- plot + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  }

  ggplot_scale_list <- list()
  scales <- rgs$scales
  for (aes in names(scales)) {
    ggplot_scale_list <- c(
      ggplot_scale_list,
      ggplot_scales(scales[[aes]], aes, rgs)
    )
  }
  for (ggplot_scale in ggplot_scale_list) {
    plot <- plot + ggplot_scale
  }

  if ("theta" %in% names(rgs$layers[[1]]$aes_mappings)) {
    plot <- plot + ggplot2::coord_polar(theta = "x")
  }

  if ("facets" %in% names(rgs)) {
    plot <- plot + ggplot_facet(rgs$facets)
  }

  plot <- plot + ggplot_labs(rgs)

  plot
}
