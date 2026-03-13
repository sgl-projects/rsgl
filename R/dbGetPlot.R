#' Generate a plot from a SGL statement
#'
#' `dbGetPlot` takes a DuckDB connection and a SGL statement
#' and returns the plot defined by the SGL statement.
#'
#' @details
#' The SGL statement is parsed into an internal graphics structure, the
#' referenced data is queried from DuckDB, semantic validation is performed,
#' any column-level transformations and aggregations are applied, and the
#' result is converted to a ggplot2 plot object.
#'
#' SGL statements support the following clauses: `visualize` (aesthetic
#' mappings), `from` (data source or SQL subquery), `using` (geom type),
#' `group by`, `collect by`, `scale by`, `facet by`, `title`, and the
#' `layer` operator for combining multiple layers.
#'
#' @param con A DuckDB connection created with [DBI::dbConnect()]
#' @param sgl_stmt A SGL statement string that defines the plot
#'
#' @return A ggplot2 plot object
#'
#' @seealso `vignette("sgl-language-guide")` for the full SGL syntax reference.
#'
#' @examples
#' library(duckdb)
#' con <- dbConnect(duckdb())
#' dbWriteTable(con, "cars", mtcars)
#'
#' # Scatterplot
#' dbGetPlot(con, "
#'   visualize
#'     hp as x,
#'     mpg as y
#'   from cars
#'   using points
#' ")
#'
#' # Histogram
#' dbGetPlot(con, "
#'   visualize
#'     bin(mpg) as x,
#'     count(*) as y
#'   from cars
#'   group by
#'     bin(mpg)
#'   using bars
#' ")
#'
#' # Scatterplot with regression line overlay
#' dbGetPlot(con, "
#'   visualize
#'     hp as x,
#'     mpg as y
#'   from cars
#'   using (
#'     points
#'     layer
#'     regression line
#'   )
#' ")
#'
#' @export
dbGetPlot <- function(con, sgl_stmt) { # nolint: object_name_linter
  rgs <- sgl_to_rgs(sgl_stmt)
  dfs <- result_dfs(rgs, con)
  validate_semantics(rgs, dfs)
  post_cta_dfs <- perform_ctas(rgs, dfs)
  rgs_to_ggplot2(rgs, post_cta_dfs)
}
