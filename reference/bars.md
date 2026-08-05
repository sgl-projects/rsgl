# Bars

Documents the aliases, aesthetics, and qualifiers for the bar geom.

## Aliases

- `bar`

- `bars`

## Aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

## Qualifiers

- `horizontal`: orients the bars horizontally.

- `unstacked`: doesn't stack overlapping bars.

- `vertical`: orients the bars vertically.

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmprCHpYD/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
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


dbGetPlot(con, "
  visualize
    bin(miles_per_gallon) as y,
    count(*) as x
  from cars
  group by
    bin(miles_per_gallon)
  using horizontal bars
")

```
