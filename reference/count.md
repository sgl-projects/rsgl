# Count

The `count` function returns the number of rows in each group.

## Function Name

- `count`

## Arguments

- `*` (required).

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)

dbGetPlot(con, "
  visualize
    origin as x,
    count(*) as y
  from cars
  group by
    origin
  using bars
")

```
