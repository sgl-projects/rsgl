# Generate a plot from a SGL statement

`dbGetPlot` takes a database connection and a SGL statement and returns
the corresponding plot.

## Usage

``` r
dbGetPlot(con, sgl_stmt)
```

## Arguments

- con:

  A database connection (as returned by DBI::dbConnect())

- sgl_stmt:

  A SGL statement (string)

## Value

The plot defined by the SGL statement (ggplot2 plot object)

## Examples

``` r
library(duckdb)
#> Loading required package: DBI
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)
p <- dbGetPlot(con, "
  visualize
    horsepower as x,
    miles_per_gallon as y
  from cars
  using points
")
print(p)
#> Warning: Removed 14 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
