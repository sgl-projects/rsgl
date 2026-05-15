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
#' @name bars
NULL
