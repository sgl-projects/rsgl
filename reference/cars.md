# Cars dataset

A sample dataset used throughout rsgl documentation and examples
containing attributes for a collection of cars. Loading rsgl masks
[datasets::cars](https://rdrr.io/r/datasets/cars.html); refer to the
built-in version as
[`datasets::cars`](https://rdrr.io/r/datasets/cars.html) if you need it.

## Usage

``` r
cars
```

## Format

A data frame with 406 rows and 5 variables:

- car_id:

  Integer identifier for the car.

- horsepower:

  Engine horsepower.

- miles_per_gallon:

  Fuel economy in miles per US gallon.

- origin:

  Country of origin (`USA`, `Europe`, `Japan`).

- year:

  Model year (1970–1982).

## Source

Derived from the cars dataset in the vega-datasets collection
(<https://github.com/vega/vega-datasets/blob/main/data/cars.json>). The
`Miles_per_Gallon`, `Horsepower`, and `Origin` columns were kept and
renamed, `year` is the year taken from the original `Year` date, and
`car_id` is a row identifier.
