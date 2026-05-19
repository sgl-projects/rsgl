#' Log
#'
#' The `log` scale maps data values to a visual property
#' through a base-10 logarithm transformation.
#'
#' @section Function Name:
#' - `log`
#'
#' @section Arguments:
#' - The name of an aesthetic to scale (required).
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", cars)
#'
#' dbGetPlot(con, "
#'   visualize
#'     horsepower as x,
#'     miles_per_gallon as y
#'   from cars
#'   using points
#'   scale by
#'     log(x),
#'     log(y)
#' ")
#'
#' @name log
NULL
