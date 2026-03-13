#' rsgl: An Implementation of the SGL Graphics Language
#'
#' Parses and executes SGL (Structured Graphics Language) statements against
#' 'DuckDB' databases to produce 'ggplot2' visualizations. SGL is a declarative
#' language for generating statistical graphics from relational data.
#'
#' @seealso
#' * [dbGetPlot()] — the main user-facing function
#' * `vignette("rsgl")` — getting started
#' * `vignette("sgl-language-guide")` — full SGL syntax reference
#' * `vignette("example-gallery")` — visual cookbook of SGL examples
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
