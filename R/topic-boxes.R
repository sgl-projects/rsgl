#' Boxes
#'
#' Documents the aliases, aesthetics, and qualifiers for the box geom.
#'
#' @section Aliases:
#' - `box`
#' - `boxes`
#'
#' @section Aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#'
#' @section Qualifiers:
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
