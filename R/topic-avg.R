#' Average
#'
#' The `avg` function returns the average of a column
#' within each group.
#'
#' @section Function Name:
#' - `avg`
#'
#' @section Arguments:
#' - The name of a numerical column to average (required).
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", cars)
#'
#' dbGetPlot(con, "
#'   visualize
#'     origin as x,
#'     avg(horsepower) as y
#'   from cars
#'   group by
#'     origin
#'   using bars
#' ")
#'
#' @name avg
NULL
