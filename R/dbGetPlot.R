#' Generate a plot from a SGL statement
#'
#' `dbGetPlot` takes a duckdb connection and a SGL statement
#' and returns the plot defined by the SGL statement.
#'
#' @param con A duckdb connection
#' @param sgl_stmt A SGL statement that defines the plot
#'
#' @return The plot defined by the SGL statement (a ggplot2 plot object)
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", mtcars)
#' p <- dbGetPlot(con, "
#'   visualize
#'     hp as x,
#'     mpg as y
#'   from cars
#'   using points
#' ")
#' print(p)
#'
#' @export
dbGetPlot <- function(con, sgl_stmt) { # nolint: object_name_linter
  rgs <- sgl_to_rgs(sgl_stmt)
  dfs <- result_dfs(rgs, con)
  validate_semantics(rgs, dfs)
  post_cta_dfs <- perform_ctas(rgs, dfs)
  rgs_to_ggplot2(rgs, post_cta_dfs)
}
