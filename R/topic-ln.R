#' Natural Log
#'
#' The `ln` scale maps data values to a visual property
#' through a natural logarithm transformation.
#'
#' @section Function Name:
#' - `ln`
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
#'     ln(x),
#'     ln(y)
#' ")
#'
#' @name ln
NULL
