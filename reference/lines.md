# Lines

The line geom lets you represent data using lines.

## Keywords

- `line`

- `lines`

## Supported aesthetics

- `x`

- `y`

- `theta`

- `r`

- `color`

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
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

```
