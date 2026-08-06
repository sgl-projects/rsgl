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
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmplOvonl/duckdb
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
    count(*) as y
  from cars
  group by
    origin
  using bars
")

```
