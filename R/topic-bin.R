#' Bin
#'
#' The `bin` function creates equal-width bins for a
#' column; original values are transformed into bin-center values.
#'
#' @section Function Name:
#' - `bin`
#'
#' @section Arguments:
#' - The name of a numerical column to bin (required).
#' - The number of bins (optional, default `30`).
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", cars)
#'
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
#'     bin(miles_per_gallon, 10) as x,
#'     count(*) as y
#'   from cars
#'   group by
#'     bin(miles_per_gallon, 10)
#'   using bars
#' ")
#'
#' @name bin
NULL
