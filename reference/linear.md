# Linear

The `linear` scale linearly maps data values to a visual property. It is
the default scale for numerical aesthetic mappings.

## Function Name

- `linear`

## Arguments

- The name of an aesthetic to scale (required).

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)

# explicit linear scales
dbGetPlot(con, "
  visualize
    horsepower as x,
    miles_per_gallon as y
  from cars
  using points
  scale by
    linear(x),
    linear(y)
")
#> Warning: Removed 14 rows containing missing values or values outside the scale range
#> (`geom_point()`).


# default linear scales
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
