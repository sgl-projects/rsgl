#' Lines
#'
#' The line geom lets you represent data using lines.
#'
#' @section Keywords:
#' - `line`
#' - `lines`
#'
#' @section Supported aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#'
#' @section Supported qualifiers:
#' - `horizontal`: orients the line horizontally;
#'  points are connected in order of increasing `x`/`theta` values.
#' - `regression`: fits a linear regression line to the data.
#' - `vertical`: orients the line vertically;
#'  points are connected in order of increasing `y`/`r` values.
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "trees", trees)
#' dbGetPlot(con, "
#'   visualize
#'     age as x,
#'     circumference as y
#'   from trees
#'   collect by
#'     tree_id
#'   using lines
#' ")
#'
#' dbWriteTable(con, "cars", cars)
#' dbGetPlot(con, "
#'   visualize
#'     horsepower as x,
#'     miles_per_gallon as y
#'   from cars
#'   using (
#'      points
#'      layer
#'      regression line
#'   )
#'    scale by
#'      log(x),
#'      log(y)
#' ")
#'
#' @name lines
NULL
