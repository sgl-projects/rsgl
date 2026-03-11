
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsgl

<!-- badges: start -->
[![R-CMD-check](https://github.com/sgl-projects/rsgl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sgl-projects/rsgl/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

rsgl implements the [SGL (Structured Graphics Language)](https://arxiv.org/abs/2505.14690), a declarative language for generating statistical graphics from relational data. See [Chapman (2025)](https://doi.org/10.48550/arXiv.2505.14690) for the full language specification.

## Installation

You can install rsgl from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("sgl-projects/rsgl")
```

## Usage

The main interface is `dbGetPlot()`, which takes a 'DuckDB' connection and a SGL statement and returns a 'ggplot2' plot object. The example below creates an in-memory 'DuckDB' database, loads data, and generates a scatterplot.

``` r
library(duckdb)
library(rsgl)

con <- dbConnect(duckdb())
dbWriteTable(con, "cars", mtcars)
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points
")
```
