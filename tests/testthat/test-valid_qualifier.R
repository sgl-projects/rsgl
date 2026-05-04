describe("valid_box_direction", {
  describe("one pos aes", {
    describe("direction and pos aes align", {
      it("doesn't raise error", {
        aligned_dirs <- c(
          x = "horizontal",
          y = "vertical",
          theta = "horizontal",
          r = "vertical"
        )
        for (aes in names(aligned_dirs)) {
          sgl <- sprintf(
            "
							visualize
								hp as %s
							from cars
							using %s box
						",
            aes,
            aligned_dirs[aes]
          )
          rgs <- sgl_to_rgs(sgl)
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_box_direction(layer)
          )
        }
      })
    })
    describe("direction and pos aes don't align", {
      it("raises error", {
        unaligned_dirs <- c(
          x = "vertical",
          y = "horizontal",
          theta = "vertical",
          r = "horizontal"
        )
        for (aes in names(unaligned_dirs)) {
          sgl <- sprintf(
            "
							visualize
								hp as %s
							from cars
							using %s box
						",
            aes,
            unaligned_dirs[aes]
          )
          rgs <- sgl_to_rgs(sgl)
          layer <- rgs$layers[[1]]

          unformatted_msg <- paste(
            "Error: a single positional aesthetic",
            "of %s does not align with the %s qualifier",
            "for the box geom."
          )
          expected_msg <- sprintf(
            unformatted_msg, aes, unaligned_dirs[aes]
          )
          expect_error(
            valid_box_direction(layer),
            expected_msg,
            fixed = TRUE
          )
        }
      })
    })
  })
  describe("two pos aes", {
    describe("no explicit collection", {
      it("doesn't raise error", {
        dirs <- c("horizontal", "vertical")
        for (dir in dirs) {
          sgl <- sprintf(
            "
							visualize
								hp as x,
								mpg as y
							from cars
							using %s boxes
						",
            dir
          )
          rgs <- sgl_to_rgs(sgl)
          layer <- rgs$layers[[1]]

          expect_no_error(
            valid_box_direction(layer)
          )
        }
      })
    })
    describe("explicit collection", {
      describe("collection does not include pos aes mappings", {
        it("doesn't raise error", {
          dirs <- c("horizontal", "vertical")
          for (dir in dirs) {
            sgl <- sprintf(
              "
								visualize
									hp as x,
									mpg as y
								from cars
								collect by
									cyl
								using %s boxes
							",
              dir
            )
            rgs <- sgl_to_rgs(sgl)
            layer <- rgs$layers[[1]]

            expect_no_error(
              valid_box_direction(layer)
            )
          }
        })
      })
      describe("collection includes both pos aes mappings", {
        it("doesn't raise error", {
          dirs <- c("horizontal", "vertical")
          for (dir in dirs) {
            sgl <- sprintf(
              "
								visualize
									hp as x,
									mpg as y
								from cars
								collect by
									hp,
									mpg
								using %s boxes
							",
              dir
            )
            rgs <- sgl_to_rgs(sgl)
            layer <- rgs$layers[[1]]

            expect_no_error(
              valid_box_direction(layer)
            )
          }
        })
      })
      describe("collection includes one pos aes mapping", {
        describe("direction aligns with uncollected pos aes", {
          it("doesn't raise error", {
            aligned_dirs <- c(
              x = "horizontal",
              y = "vertical",
              theta = "horizontal",
              r = "vertical"
            )
            for (aes in names(aligned_dirs)) {
              if (aes %in% .cart_aes) {
                other_aes <- setdiff(.cart_aes, aes)
              } else {
                other_aes <- setdiff(.polar_aes, aes)
              }
              sgl <- sprintf(
                "
									visualize
										hp as %s,
										mpg as %s
									from cars
									collect by
										mpg
									using %s boxes
								",
                aes,
                other_aes,
                aligned_dirs[aes]
              )
              rgs <- sgl_to_rgs(sgl)
              layer <- rgs$layers[[1]]

              expect_no_error(
                valid_box_direction(layer)
              )
            }
          })
        })
        describe("direction doesn't align with uncollected pos aes", {
          it("raises error", {
            unaligned_dirs <- c(
              x = "vertical",
              y = "horizontal",
              theta = "vertical",
              r = "horizontal"
            )
            for (aes in names(unaligned_dirs)) {
              if (aes %in% .cart_aes) {
                other_aes <- setdiff(.cart_aes, aes)
              } else {
                other_aes <- setdiff(.polar_aes, aes)
              }
              sgl <- sprintf(
                "
									visualize
										hp as %s,
										mpg as %s
									from cars
									collect by
										mpg
									using %s boxes
								",
                aes,
                other_aes,
                unaligned_dirs[aes]
              )
              rgs <- sgl_to_rgs(sgl)
              layer <- rgs$layers[[1]]

              unformatted_msg <- paste(
                "Error: a single uncollected positional aesthetic",
                "of %s does not align with the %s qualifier",
                "for the box geom."
              )
              expected_msg <- sprintf(
                unformatted_msg, aes, unaligned_dirs[aes]
              )
              expect_error(
                valid_box_direction(layer),
                expected_msg,
                fixed = TRUE
              )
            }
          })
        })
      })
    })
  })
})

describe("valid_qualifier", {
  it("considers default valid", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using points
		")
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer)
    )
  })
  it("considers valid qualifier valid", {
    rgs <- sgl_to_rgs("
				visualize
					hp as x,
					mpg as y
				from cars
				using jittered points
			")
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer)
    )
  })
  it("considers invalid qualifier invalid", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			using regression boxes
		")
    layer <- rgs$layers[[1]]

    expect_error(
      valid_qualifier(layer),
      "Error: the regression qualifier is not valid for the box geom.",
      fixed = TRUE
    )
  })
  it("performs additional checks for box direction", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			collect by
				hp
			using horizontal boxes
		")
    layer <- rgs$layers[[1]]

    expected_msg <- paste(
      "Error: a single uncollected positional aesthetic",
      "of y does not align with the horizontal qualifier",
      "for the box geom."
    )
    expect_error(
      valid_qualifier(layer),
      expected_msg,
      fixed = TRUE
    )
  })
  it("doesn't perform additional checks for non-box", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				mpg as y
			from cars
			collect by
				hp
			using horizontal lines
		")
    layer <- rgs$layers[[1]]

    expect_no_error(
      valid_qualifier(layer)
    )
  })
})
