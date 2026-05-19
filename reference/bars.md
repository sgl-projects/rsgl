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

## Supported qualifiers

- `horizontal`: orients the bars horizontally.

- `unstacked`: doesn't stack overlapping bars.

- `vertical`: orients the bars vertically.

## Examples

``` r
library(duckdb)
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


dbGetPlot(con, "
  visualize
    bin(miles_per_gallon) as y,
    count(*) as x
  from cars
  group by
    bin(miles_per_gallon)
  using horizontal bars
")
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_bar()`).

```
