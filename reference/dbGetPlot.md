# Generate a plot from a SGL statement

`dbGetPlot` takes a DBI connection and a SGL statement and returns the
corresponding plot.

## Usage

``` r
dbGetPlot(con, sgl_stmt)
```

## Arguments

- con:

  A DBI connection

- sgl_stmt:

  A SGL statement (string)

## Value

The plot defined by the SGL statement (ggplot2 plot object)

## Examples

``` r
library(duckdb)
#> Loading required package: DBI
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", mtcars)
p <- dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points
")
print(p)

```
