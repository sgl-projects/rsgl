# Bin

The `bin` function creates equal-width bins for a numerical column.
Original values are transformed into binned values. By default 30 bins
are created, but the number of bins can be explicitly set via an
optional argument.

## Function Name

- `bin`

## Arguments

- A numerical column to bin (required).

- An positive integer specifying the number of bins (optional, default
  `30`).

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
    bin(miles_per_gallon, 10) as x,
    count(*) as y
  from cars
  group by
    bin(miles_per_gallon, 10)
  using bars
")
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_bar()`).

```
