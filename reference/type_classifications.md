# Get SGL type classificatons for columns in a table

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
dbWriteTable(con, "iris", iris)
type_classifications(con, "iris")
#>    column_name column_class
#> 1 Sepal.Length    numerical
#> 2  Sepal.Width    numerical
#> 3 Petal.Length    numerical
#> 4  Petal.Width    numerical
#> 5      Species  categorical
```
