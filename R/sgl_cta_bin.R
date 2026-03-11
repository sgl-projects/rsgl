new_sgl_cta_bin <- function() {
  new_sgl_cta(class = "sgl_cta_bin")
}

#' @export
valid_cta.sgl_cta_bin <- function(cta, col_name, df) {
  if (col_name == "*") {
    stop("Error: '*' can only be used inside an aggregation function")
  }
  col <- df[[col_name]]
  if (is_categorical_col(col)) {
    errmsg <- sprintf(
      "Error: cannot apply bin to a categorical column, found bin(%s).",
      col_name
    )
    stop(errmsg)
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
    new_col_name <- paste(
      "rsgl", scale_name(scale), "bin", input_col_name,
      sep = "."
    )
    if (new_col_name %in% colnames(df)) {
      return(df)
    }
    new_col_values <- bin_values(df[[input_col_name]], 5, scale)
    df[[new_col_name]] <- new_col_values
    df
  }

#' @export
is_transformation.sgl_cta_bin <- function(cta) {
  TRUE
}
