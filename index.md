# rsgl

rsgl implements the [SGL graphics
language](https://arxiv.org/pdf/2505.14690) for use within R. SGL is a
graphics language that is designed to look and feel like SQL, and is
based on the grammar of graphics.

## Installation

To install the official [CRAN](https://cran.r-project.org) release:

``` r

install.packages("rsgl")
```

If you instead want the development version, install from GitHub with:

``` r

# install.packages("devtools")
devtools::install_github("sgl-projects/rsgl")
```

## Usage

`dbGetPlot` is the primary interface to rsgl. It takes a
[DBI](https://dbi.r-dbi.org) database connection and a SGL statement and
returns the corresponding plot.

The following example demonstrates creating a DBI connection to an
in-memory [DuckDB](https://duckdb.org) database, loading it with data,
and then generating a scatterplot from a SGL statement.

``` r

library(duckdb)
#> Loading required package: DBI
library(rsgl)
#> 
#> Attaching package: 'rsgl'
#> The following objects are masked from 'package:datasets':
#> 
#>     cars, trees

con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)

dbGetPlot(con, "
    visualize
        horsepower as x,
        miles_per_gallon as y
    from cars
    using points
")
```

![](reference/figures/README-usage-1.png)

## Next steps

- [Get started](https://sgl-projects.github.io/rsgl/articles/rsgl.html)
  — a tutorial of rsgl and the SGL language.
- [Example
  gallery](https://sgl-projects.github.io/rsgl/articles/example-gallery.html)
  — a collection of plots generated with rsgl.
