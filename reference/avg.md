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
#> duckdb is keeping downloaded extensions in a temporary directory:
#> ℹ /tmp/RtmphlqCsb/duckdb/extensions
#> This is removed when the R session ends, so extensions are re-downloaded each session.
#> ℹ To keep them, point `options(duckdb.extension_directory =)` or the `DUCKDB_EXTENSION_DIRECTORY` environment variable at a permanent path.
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
