# Get SGL type classifications for columns in a table

`type_classifications` takes a database connection and a table name and
returns the SGL type classifications (numerical, categorical, or
temporal) of the table's columns.

## Usage

``` r
type_classifications(con, table_name)
```

## Arguments

- con:

  A database connection (as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html))

- table_name:

  The name of a table

## Value

A dataframe listing the SGL type classification of each column.

## Examples

``` r
library(duckdb)
con <- dbConnect(duckdb())
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpF85TT5/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
dbWriteTable(con, "iris", iris)
type_classifications(con, "iris")
#>    column_name column_class
#> 1 Sepal.Length    numerical
#> 2  Sepal.Width    numerical
#> 3 Petal.Length    numerical
#> 4  Petal.Width    numerical
#> 5      Species  categorical
```
