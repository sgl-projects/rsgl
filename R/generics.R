valid_aesthetics <- function(geom, layer) {
  UseMethod("valid_aesthetics")
}

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

valid_qualifier <- function(geom, layer) {
  UseMethod("valid_qualifier")
}

is_collective <- function(geom) {
  UseMethod("is_collective")
}

valid_collections <- function(geom, layer, df) {
  UseMethod("valid_collections")
}

valid_cta <- function(cta, col_name, df) {
  UseMethod("valid_cta")
}

cta_name <- function(cta) {
  UseMethod("cta_name")
}

add_transformed_column <- function(cta, input_col_name, df, ...) {
  UseMethod("add_transformed_column")
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
