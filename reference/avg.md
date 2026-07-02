# Average

The `avg` function returns the average of a column within each group.

## Function Name

- `avg`

## Arguments

- The name of a numerical column to average (required).

## Examples

``` r
library(duckdb)
#> Loading required package: DBI
con <- dbConnect(duckdb())
dbWriteTable(con, "cars", cars)

dbGetPlot(con, "
  visualize
    origin as x,
    avg(horsepower) as y
  from cars
  group by
    origin
  using bars
")

```
