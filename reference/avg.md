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
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpMm0YzW/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
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
