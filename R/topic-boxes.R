#' Boxes
#'
#' The box geom lets you represent data using boxplot box objects.
#'
#' @section Keywords:
#' - `box`
#' - `boxes`
#'
#' @section Supported aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#'
#' @section Supported qualifiers:
#' - `horizontal`: orients the boxes horizontally.
#' - `vertical`: orients the boxes vertically.
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", cars)
#' dbGetPlot(con, "
#'   visualize
#'     origin as x,
#'     miles_per_gallon as y
#'   from cars
#'   using boxes
#' ")
#'
#' dbGetPlot(con, "
#'   visualize
#'     origin as y,
#'     miles_per_gallon as x
#'   from cars
#'   using horizontal boxes
#' ")
#'
#' @name boxes
NULL
