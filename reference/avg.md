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
#> duckdb: caching downloaded extensions in the package library:
#> ℹ /home/runner/work/_temp/Library/duckdb/extensions
#> ℹ This is removed when the package is re-installed; see `?duckdb_storage` to choose a different location.
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
