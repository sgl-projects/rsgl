# Boxes

Documents the aliases, aesthetics, and qualifiers for the box geom.

## Aliases

- `box`

- `boxes`

## Aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

## Qualifiers

- `horizontal`: orients the boxes horizontally.

- `vertical`: orients the boxes vertically.

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpRvPSdz/duckdb
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
    miles_per_gallon as y
  from cars
  using boxes
")


dbGetPlot(con, "
  visualize
    origin as y,
    miles_per_gallon as x
  from cars
  using horizontal boxes
")

```
