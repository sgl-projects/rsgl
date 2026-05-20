#' Points
#'
#' Documents the aliases, aesthetics, and qualifiers for the point geom.
#'
#' @section Aliases:
#' - `point`
#' - `points`
#'
#' @section Aesthetics:
#' - `x`
#' - `y`
#' - `theta`
#' - `r`
#' - `color`
#' - `size`
#'
#' @section Qualifiers:
#' - `jittered`: adds a small amount of random noise to each point's position.
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", cars)
#' dbGetPlot(con, "
#'   visualize
#'     horsepower as x,
#'     miles_per_gallon as y
#'   from cars
#'   using points
#' ")
#'
#' set.seed(0)
#' dbGetPlot(con, "
#'   visualize
#'     origin as x,
#'     miles_per_gallon as y
#'   from cars
#'   using jittered points
#' ")
#'
#' @name points
NULL
