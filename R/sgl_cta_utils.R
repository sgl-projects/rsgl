raise_if_arg_present <- function(col_expr) {
  if ("arg" %in% names(col_expr)) {
    err_msg <- sprintf(
      "Error: %s function received unexpected argument.",
      cta_fn_name(col_expr$cta)
    )
    stop(err_msg)
  }
}
