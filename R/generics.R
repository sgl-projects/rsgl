geom_name <- function(geom) {
  UseMethod("geom_name")
}

ggplot_geom <- function(geom) {
  UseMethod("ggplot_geom")
}

ggplot_aes <- function(geom, layer, df, scales) {
  UseMethod("ggplot_aes")
}

valid_qual_list <- function(geom) {
  UseMethod("valid_qual_list")
}

is_collective <- function(geom) {
  UseMethod("is_collective")
}

extension <- function(geom) {
  UseMethod("extension")
}

valid_cta <- function(cta, col_expr, df) {
  UseMethod("valid_cta")
}

cta_fn_name <- function(cta) {
  UseMethod("cta_fn_name")
}

add_transformed_column <- function(cta, input_col_name, df, ...) {
  UseMethod("add_transformed_column")
}

is_aggregation <- function(cta) {
  UseMethod("is_aggregation")
}

is_transformation <- function(cta) {
  UseMethod("is_transformation")
}

scale_name <- function(scale) {
  UseMethod("scale_name")
}

valid_scale <- function(scale, aes, layers, dfs) {
  UseMethod("valid_scale")
}

apply_scale <- function(scale, values) {
  UseMethod("apply_scale")
}

apply_scale_inverse <- function(scale, values) {
  UseMethod("apply_scale_inverse")
}

ggplot_scales <- function(scale, aes, rgs) {
  UseMethod("ggplot_scales")
}

valid_non_pos_aes <- function(geom) {
  UseMethod("valid_non_pos_aes")
}

has_direction <- function(geom) {
  UseMethod("has_direction")
}

ggplot_dir_from_qual <- function(geom, qual) {
  UseMethod("ggplot_dir_from_qual")
}

agg_col_name <- function(cta, col_expr, scale) {
  UseMethod("agg_col_name")
}

agg_col_expr <- function(cta, col_expr, scale) {
  UseMethod("agg_col_expr")
}

expr_text <- function(cta, col_expr) {
  UseMethod("expr_text")
}
