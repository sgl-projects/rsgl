#' rsgl: An Implementation of the SGL Graphics Language
#'
#' Parses and executes SGL (Structured Graphics Language) statements against
#' 'DuckDB' databases to produce 'ggplot2' visualizations. SGL is a declarative
#' language for generating statistical graphics from relational data.
#'
#' @seealso [dbGetPlot()] for the main user-facing function.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib rsgl, .registration = TRUE
## usethis namespace: end
NULL
