#' Cars dataset
#'
#' A sample dataset used throughout rsgl documentation and examples
#' containing attributes for a collection of cars.
#' Loading rsgl masks [datasets::cars]; refer to the built-in version as
#' `datasets::cars` if you need it.
#'
#' @format A data frame with 406 rows and 5 variables:
#' \describe{
#'   \item{car_id}{Integer identifier for the car.}
#'   \item{horsepower}{Engine horsepower.}
#'   \item{miles_per_gallon}{Fuel economy in miles per US gallon.}
#'   \item{origin}{Country of origin (`USA`, `Europe`, `Japan`).}
#'   \item{year}{Model year (1970–1982).}
#' }
#' @source Derived from the cars dataset in the vega-datasets collection
#'   (<https://github.com/vega/vega-datasets/blob/main/data/cars.json>). The
#'   `Miles_per_Gallon`, `Horsepower`, and `Origin` columns were kept and
#'   renamed, `year` is the year taken from the original `Year` date, and
#'   `car_id` is a row identifier.
"cars"
