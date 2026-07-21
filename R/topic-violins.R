#' Violins
#'
#' Documents the aliases, aesthetics, and qualifiers for the violin geom.
#'
#' @section Aliases:
#' - `violin`
#' - `violins`
#'
#' @section Aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#'
#' @section Qualifiers:
#' - `horizontal`: orients the violins horizontally.
#' - `vertical`: orients the violins vertically.
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
#'   using violins
#' ")
#'
#' dbGetPlot(con, "
#'   visualize
#'     origin as y,
#'     miles_per_gallon as x
#'   from cars
#'   using horizontal violins
#' ")
#'
#' @name violins
NULL
