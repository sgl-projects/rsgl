new_sgl_cta_bin <- function() {
  new_sgl_cta(class = "sgl_cta_bin")
}

#' @export
cta_fn_name.sgl_cta_bin <- function(cta) {
  "bin"
}

#' @export
valid_cta.sgl_cta_bin <- function(cta, col_expr, df) {
  col_name <- col_expr$column
  if (col_name == "*") {
    stop("Error: '*' can only be used inside an aggregation function")
  }
  col <- df[[col_name]]
  if (is_categorical_col(col)) {
    bin_fn_name <- cta_fn_name(cta)
    errmsg <- sprintf(
      "Error: cannot apply %s to a categorical column, found %s(%s).",
      bin_fn_name,
      bin_fn_name,
      col_name
    )
    stop(errmsg)
  }

  if ("arg" %in% names(col_expr)) {
    if (col_expr$arg <= 0) {
      stop("Error: number of bins must be greater than 0.")
    }
  }
}

#' @export
add_transformed_column.sgl_cta_bin <-
  function(cta, input_col_name, df, ...) {
    dots <- list(...)
    if (!("scale" %in% names(dots))) {
      stop("Error: expected argument 'scale' to be provided.")
    }
    scale <- dots$scale
    if ("num_bins" %in% names(dots)) {
      num_bins <- dots$num_bins
    } else {
      num_bins <- 30
    }
    new_col_name <- paste(
      "rsgl", scale_name(scale), cta_fn_name(cta), num_bins, input_col_name,
      sep = "."
    )
    if (new_col_name %in% colnames(df)) {
      return(df)
    }
    new_col_values <- bin_values(df[[input_col_name]], num_bins, scale)
    df[[new_col_name]] <- new_col_values
    df
  }

#' @export
is_aggregation.sgl_cta_bin <- function(cta) {
  FALSE
}

#' @export
is_transformation.sgl_cta_bin <- function(cta) {
  TRUE
}

#' @export
expr_text.sgl_cta_bin <- function(cta, col_expr) {
  bin_fn_name <- cta_fn_name(cta)
  if ("arg" %in% names(col_expr)) {
    title <- sprintf(
      "%s(%s, %s)",
      bin_fn_name,
      col_expr$column,
      col_expr$arg
    )
    return(title)
  }
  sprintf(
    "%s(%s)",
    bin_fn_name,
    col_expr$column
  )
}
