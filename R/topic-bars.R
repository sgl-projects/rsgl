#' Bars
#'
#' Documents the aliases, aesthetics, and qualifiers for the bar geom.
#'
#' @section Aliases:
#' - `bar`
#' - `bars`
#'
#' @section Aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#'
#' @section Qualifiers:
#' - `horizontal`: orients the bars horizontally.
#' - `unstacked`: doesn't stack overlapping bars.
#' - `vertical`: orients the bars vertically.
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", cars)
#' dbGetPlot(con, "
#'   visualize
#'     bin(miles_per_gallon) as x,
#'     count(*) as y
#'   from cars
#'   group by
#'     bin(miles_per_gallon)
#'   using bars
#' ")
#'
#' dbGetPlot(con, "
#'   visualize
#'     bin(miles_per_gallon) as y,
#'     count(*) as x
#'   from cars
#'   group by
#'     bin(miles_per_gallon)
#'   using horizontal bars
#' ")
#'
#' @name bars
NULL
