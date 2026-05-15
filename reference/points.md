# Points

The point geom lets you represent data using point objects.

## Keywords

- `point`

- `points`

## Supported aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

- `size`

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)
dbGetPlot(con, "
  visualize
    horsepower as x,
    miles_per_gallon as y
  from cars
  using points
")
#> Warning: Removed 14 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
