# Log

The `log` scale maps data values to a visual property through a base-10
logarithm transformation.

## Function Name

- `log`

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
    log(x),
    log(y)
")

```
