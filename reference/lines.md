# Lines

Documents the aliases, aesthetics, and qualifiers for the line geom.

## Aliases

- `line`

- `lines`

## Aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

## Qualifiers

- `horizontal`: orients the line horizontally; points are connected in
  order of increasing `x`/`theta` values.

- `regression`: fits a linear regression line to the data.

- `vertical`: orients the line vertically; points are connected in order
  of increasing `y`/`r` values.

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
dbWriteTable(con, "trees", trees)
dbGetPlot(con, "
  visualize
    age as x,
    circumference as y
  from trees
  collect by
    tree_id
  using lines
")


dbWriteTable(con, "cars", cars)
dbGetPlot(con, "
  visualize
    horsepower as x,
    miles_per_gallon as y
  from cars
  using (
     points
     layer
     regression line
  )
   scale by
     log(x),
     log(y)
")

```
