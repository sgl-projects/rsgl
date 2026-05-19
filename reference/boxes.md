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

## Supported qualifiers

- `horizontal`: orients the boxes horizontally.

- `vertical`: orients the boxes vertically.

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


dbGetPlot(con, "
  visualize
    origin as y,
    miles_per_gallon as x
  from cars
  using horizontal boxes
")
#> Warning: Removed 8 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).

```
