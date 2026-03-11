col_expr_has_cta <-
  function(col_expr, cta_name = c("identity", "bin", "count")) {
    cta_name <- match.arg(cta_name)

    cta_name_to_class_mapping <- list(
      identity = new_sgl_cta_identity(),
      bin = new_sgl_cta_bin(),
      count = new_sgl_cta_count()
    )

    identical(cta_name_to_class_mapping[[cta_name]], col_expr$cta)
  }

filter_col_exprs_by_cta <-
  function(col_exprs, cta_name = c("identity", "bin", "count")) {
    cta_name <- match.arg(cta_name)

    purrr::keep(
      col_exprs,
      ~ col_expr_has_cta(.x, cta_name)
    )
  }
