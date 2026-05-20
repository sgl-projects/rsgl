# Points

Documents the aliases, aesthetics, and qualifiers for the point geom.

## Aliases

- `point`

- `points`

## Aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

- `size`

## Qualifiers

- `jittered`: adds a small amount of random noise to each point's
  position.

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


set.seed(0)
dbGetPlot(con, "
  visualize
    origin as x,
    miles_per_gallon as y
  from cars
  using jittered points
")
#> Warning: Removed 8 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
