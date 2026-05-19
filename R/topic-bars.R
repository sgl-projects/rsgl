#' Bars
#'
#' The bar geom lets you represent data using rectangular bar objects.
#'
#' @section Keywords:
#' - `bar`
#' - `bars`
#'
#' @section Supported aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#'
#' @section Supported qualifiers:
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
