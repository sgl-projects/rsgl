# Bars

The bar geom lets you represent data using rectangular bar objects.

## Keywords

- `bar`

- `bars`

## Supported aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

## Examples

``` r
library(duckdb)
#> Loading required package: DBI
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)
dbGetPlot(con, "
  visualize
    bin(miles_per_gallon) as x,
    count(*) as y
  from cars
  group by
    bin(miles_per_gallon)
  using bars
")
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_bar()`).

```
