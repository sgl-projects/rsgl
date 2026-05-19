# Natural Log

The `ln` scale maps data values to a visual property through a natural
logarithm transformation.

## Function Name

- `ln`

## Arguments

- The name of an aesthetic to scale (required).

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
  scale by
    ln(x),
    ln(y)
")
#> Warning: Removed 14 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
