# Boxes

The box geom lets you represent data using boxplot box objects.

## Keywords

- `box`

- `boxes`

## Supported aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)
dbGetPlot(con, "
  visualize
    origin as x,
    miles_per_gallon as y
  from cars
  using boxes
")
#> Warning: Removed 8 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).

```
