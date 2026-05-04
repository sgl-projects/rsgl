describe("valid_titles", {
  describe("no explicit titles", {
    describe("single layer", {
      it("doesn't raise error", {
        rgs <- sgl_to_rgs("
					visualize
						mpg as x
					from cars
					using points
				")

        expect_no_error(
          valid_titles(rgs)
        )
      })
    })
    describe("multiple layers", {
      it("doesn't raise error", {
        rgs <- sgl_to_rgs("
					visualize
						mpg as x
					from cars
					using points

					layer

					visualize
						hp as x
					from cars
					using points
				")

        expect_no_error(
          valid_titles(rgs)
        )
      })
    })
  })
  describe("single explicit title", {
    describe("single layer", {
      it("doesn't raise error if titled aes is in mapping", {
        rgs <- sgl_to_rgs("
					visualize
						mpg as x
					from cars
					using points
					title
						x as 'Miles Per Gallon'
				")

        expect_no_error(
          valid_titles(rgs)
        )
      })
      it("raises error if titled aes is not in mapping", {
        rgs <- sgl_to_rgs("
					visualize
						mpg as x
					from cars
					using points
					title
						y as 'Miles Per Gallon'
				")

        expected_msg <- paste(
          "Error: title provided for aesthetic not found",
          "in any layer's aesthetic mapping: y."
        )
        expect_error(
          valid_titles(rgs),
          expected_msg,
          fixed = TRUE
        )
      })
    })
    describe("multiple layers", {
      it(
        "doesn't raise error if titled aes is in at least one layer's mapping",
        {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y,
							cyl as color
						from cars
						using points

						layer

						visualize
							hp as x,
							mpg as y
						from cars
						using regression line

						title
							color as 'Cylinders'
					")

          expect_no_error(
            valid_titles(rgs)
          )
        }
      )
      it("raises error if titled aes is not in any layer's mapping", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x,
						mpg as y
					from cars
					using points

					layer

					visualize
						hp as x,
						mpg as y
					from cars
					using regression line

					title
						color as 'Cylinders'
				")

        expected_msg <- paste(
          "Error: title provided for aesthetic not found",
          "in any layer's aesthetic mapping: color."
        )
        expect_error(
          valid_titles(rgs),
          expected_msg,
          fixed = TRUE
        )
      })
    })
  })
  describe("multiple explicit titles", {
    describe("single layer", {
      it(
        "doesn't raise error for multiple titled aes that are in mapping",
        {
          rgs <- sgl_to_rgs("
						visualize
							hp as x,
							mpg as y
						from cars
						using points
						title
							x as 'Horsepower',
							y as 'Miles Per Gallon'
					")

          expect_no_error(
            valid_titles(rgs)
          )
        }
      )
      it("raises error if one titled aes is not in mapping", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x,
						mpg as y
					from cars
					using points
					title
						x as 'Horsepower',
						y as 'Miles Per Gallon',
						color as 'Cylinders'
				")

        expected_msg <- paste(
          "Error: title provided for aesthetic not found",
          "in any layer's aesthetic mapping: color."
        )
        expect_error(
          valid_titles(rgs),
          expected_msg,
          fixed = TRUE
        )
      })
      it("raises error if multiple titled aes are not in mapping", {
        rgs <- sgl_to_rgs("
					visualize
						mpg as x
					from cars
					using points
					title
						y as 'Miles Per Gallon',
						color as 'Cylinders'
				")

        expected_msg <- paste(
          "Error: title provided for aesthetic not found",
          "in any layer's aesthetic mapping: color."
        )
        expect_error(
          valid_titles(rgs),
          expected_msg,
          fixed = TRUE
        )
      })
    })
    describe("multiple layers", {
      it(
        "doesn't raise error for multiple titled aes that are in some mapping",
        {
          rgs <- sgl_to_rgs("
						visualize
							hp as x
						from cars
						using points

						layer

						visualize
							mpg as y
						from cars
						using points

						title
							x as 'Horsepower',
							y as 'Miles Per Gallon'
					")

          expect_no_error(
            valid_titles(rgs)
          )
        }
      )
      it("raises error if one titled aes is not in any mapping", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x
					from cars
					using points

					layer

					visualize
						mpg as y
					from cars
					using points

					title
						x as 'Horsepower',
						y as 'Miles Per Gallon',
						color as 'Cylinders'
				")

        expected_msg <- paste(
          "Error: title provided for aesthetic not found",
          "in any layer's aesthetic mapping: color."
        )
        expect_error(
          valid_titles(rgs),
          expected_msg,
          fixed = TRUE
        )
      })
      it("raises error if multiple titled aes are not in any mapping", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x
					from cars
					using points

					layer

					visualize
						mpg as x
					from cars
					using points

					title
						x as 'Horsepower',
						y as 'Miles Per Gallon',
						color as 'Cylinders'
				")

        expected_msg <- paste(
          "Error: title provided for aesthetic not found",
          "in any layer's aesthetic mapping: color."
        )
        expect_error(
          valid_titles(rgs),
          expected_msg,
          fixed = TRUE
        )
      })
    })
  })
})
