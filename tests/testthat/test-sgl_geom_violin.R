test_violin <- new_sgl_geom_violin()

test_that(
  "new_sgl_geom_violin returns an instance of the sgl_geom_violin subclass",
  {
    expect_equal(
      attr(test_violin, "class"),
      c("sgl_geom_violin", "sgl_geom")
    )
  }
)

test_that("new_sgl_geom_violin only adds class attribute", {
  all_attributes <- names(attributes(test_violin))
  expect_equal(all_attributes, "class")
})

test_that("new_sgl_geom_violin sets empty list as base object", {
  base_object <- unclass(test_violin)

  expect_equal(base_object, list())
})

test_that("geom_name returns violin", {
  actual <- geom_name(test_violin)
  expect_equal(actual, "violin")
})

test_that("is_collective returns TRUE", {
  expect_equal(is_collective(test_violin), TRUE)
})

test_that("ggplot_geom returns geom_violin function", {
  actual <- ggplot_geom(test_violin)
  expect_equal(actual, ggplot2::geom_violin)
})

describe("group_aes_cols", {
  describe("single pos aes", {
    it("doesn't include pos aes mapping", {
      rgs <- sgl_to_rgs("
        visualize
          price as y
        from diamonds
        using violin
      ")
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]
      layer <- rgs$layers[[1]]

      expect_setequal(
        group_aes_cols(
          layer$geom_expr$geom,
          layer,
          df,
          rgs$scales
        ),
        character(0)
      )
    })
  })
  describe("two pos aes", {
    describe("direction qualifier is present", {
      describe("qualifier is vertical", {
        describe("cart coords", {
          it("returns x mapping", {
            rgs <- sgl_to_rgs("
              visualize
                cut as x,
                price as y
              from diamonds
              using vertical violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "cut"
            )
          })
        })
        describe("polar coords", {
          it("returns theta mapping", {
            rgs <- sgl_to_rgs("
              visualize
                cut as theta,
                price as r
              from diamonds
              using vertical violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "cut"
            )
          })
        })
      })
      describe("qualifier is horizontal", {
        describe("cart coords", {
          it("returns y mapping", {
            rgs <- sgl_to_rgs("
              visualize
                cut as x,
                price as y
              from diamonds
              using horizontal violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "price"
            )
          })
        })
        describe("polar coords", {
          it("returns r mapping", {
            rgs <- sgl_to_rgs("
              visualize
                cut as theta,
                price as r
              from diamonds
              using horizontal violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "price"
            )
          })
        })
      })
    })
    describe("direction qualifier is not present", {
      describe("cart coords", {
        describe("x has orientation priority", {
          it("returns x mapping", {
            rgs <- sgl_to_rgs("
              visualize
                cut as x,
                price as y
              from diamonds
              using violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "cut"
            )
          })
        })
        describe("y has orientation priority", {
          it("returns y mapping", {
            rgs <- sgl_to_rgs("
              visualize
                price as x,
                cut as y
              from diamonds
              using violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "cut"
            )
          })
        })
      })
      describe("polar coords", {
        describe("theta has orientation priority", {
          it("returns theta mapping", {
            rgs <- sgl_to_rgs("
              visualize
                cut as theta,
                price as r
              from diamonds
              using violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "cut"
            )
          })
        })
        describe("r has orientation priority", {
          it("returns r mapping", {
            rgs <- sgl_to_rgs("
              visualize
                price as theta,
                cut as r
              from diamonds
              using violins
            ")
            dfs <- result_dfs(rgs, test_con)
            df <- dfs[[1]]
            layer <- rgs$layers[[1]]

            actual <- group_aes_cols(
              layer$geom_expr$geom,
              layer,
              df,
              rgs$scales
            )

            expect_setequal(
              actual,
              "cut"
            )
          })
        })
      })
    })
  })
  it("includes non-pos mapping", {
    rgs <- sgl_to_rgs("
      visualize
        cut as x,
        price as y,
        clarity as color
      from diamonds
      using violins
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual <- group_aes_cols(
      layer$geom_expr$geom,
      layer,
      df,
      rgs$scales
    )

    expect_setequal(
      actual,
      c("cut", "clarity")
    )
  })
  it("uses post-cta columns", {
    rgs <- sgl_to_rgs("
      visualize
        bin(carat) as x,
        price as y,
        bin(clarity,5) as color
      from diamonds
      using violins

      scale by
        log(x)
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual <- group_aes_cols(
      layer$geom_expr$geom,
      layer,
      df,
      rgs$scales
    )

    expect_setequal(
      actual,
      c("rsgl.log.bin.30.carat", "rsgl.linear.bin.5.clarity")
    )
  })
  it("doesn't add duplicates", {
    rgs <- sgl_to_rgs("
      visualize
        cut as x,
        price as y,
        cut as color
      from diamonds
      using violins

      scale by
        log(x)
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual <- group_aes_cols(
      layer$geom_expr$geom,
      layer,
      df,
      rgs$scales
    )

    expect_setequal(
      actual,
      "cut"
    )
  })
})

test_that("ggplot_aes returns correct aes mappings", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y,
      clarity as color
    from diamonds
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "cut")
  expect_equal(ggplot2::as_label(actual_aes$y), "price")
  expect_equal(ggplot2::as_label(actual_aes$colour), "clarity")
})

test_that("ggplot_aes maps to cta generated columns correctly", {
  rgs <- sgl_to_rgs("
    visualize
      bin(mpg) as x,
      count(*) as y,
      bin(hp, 5) as color
    from cars
    group by
      bin(mpg),
      bin(hp)
    using violins
    scale by
      log(x)
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(class(actual_aes)[1], "ggplot2::mapping")
  expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.log.bin.30.mpg")
  expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
  expect_equal(ggplot2::as_label(actual_aes$colour), "rsgl.linear.bin.5.hp")
})

test_that(
  paste(
    "ggplot_aes maps to mix of cta generated and",
    "non cta generated columns correctly"
  ),
  {
    rgs <- sgl_to_rgs("
      visualize
        bin(mpg) as x,
        count(*) as y,
        vs_cat as color
      from (
        select
          *,
          cast(vs as varchar) as vs_cat
        from cars
      )
      group by
        bin(mpg),
        vs_cat
      using violins
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_equal(class(actual_aes)[1], "ggplot2::mapping")
    expect_equal(names(actual_aes), c("x", "y", "colour", "group"))
    expect_equal(ggplot2::as_label(actual_aes$x), "rsgl.linear.bin.30.mpg")
    expect_equal(ggplot2::as_label(actual_aes$y), "rsgl.count")
    expect_equal(ggplot2::as_label(actual_aes$colour), "vs_cat")
  }
)

test_that("ggplot_aes replaces theta and r with x and y", {
  rgs <- sgl_to_rgs("
    visualize
      cut as theta,
      price as r
    from diamonds
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y", "group"))
  expect_equal(ggplot2::as_label(actual_aes$x), "cut")
  expect_equal(ggplot2::as_label(actual_aes$y), "price")
})

test_that("ggplot_aes adds blank mapping for omitted y aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as x
    from cars
    using violin
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "mpg")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted x aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as y
    from cars
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes adds blank mapping for omitted r aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as theta
    from cars
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(ggplot2::as_label(actual_aes$x), "mpg")
  expect_equal(actual_aes$y, "")
})

test_that("ggplot_aes adds blank mapping for omitted theta aesthetic", {
  rgs <- sgl_to_rgs("
    visualize
      mpg as r
    from cars
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_equal(names(actual_aes), c("x", "y"))
  expect_equal(actual_aes$x, "")
  expect_equal(ggplot2::as_label(actual_aes$y), "mpg")
})

test_that("ggplot_aes includes explicit collection in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
    collect by
      cut
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "cut")
})

test_that("ggplot_aes includes multiple explicit collections in group aes", {
  rgs <- sgl_to_rgs("
    visualize
      cut as x,
      price as y
    from diamonds
    collect by
      cut,
      clarity
    using violins
  ")
  dfs <- result_dfs(rgs, test_con)
  df <- dfs[[1]]
  layer <- rgs$layers[[1]]

  actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

  expect_true("group" %in% names(actual_aes))
  expect_equal(ggplot2::as_label(actual_aes$group), "interaction(clarity, cut)")
})

test_that(
  "ggplot_aes includes unmapped transformed explicit collection in group aes",
  {
    rgs <- sgl_to_rgs("
      visualize
        cut as x,
        price as y
      from diamonds
      collect by
        cut,
        bin(carat, 5)
      using violins
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(rsgl.linear.bin.5.carat, cut)"
    )
  }
)

test_that(
  "ggplot_aes includes mapped transformed explicit collection in group aes",
  {
    rgs <- sgl_to_rgs("
      visualize
        bin(carat) as x,
        price as y
      from diamonds
      collect by
        bin(carat)
      using violins
      scale by
        log(x)
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(ggplot2::as_label(actual_aes$group), "rsgl.log.bin.30.carat")
  }
)

test_that(
  paste(
    "ggplot_aes includes both scales for mapped",
    "transformed explicit collection in group aes"
  ),
  {
    rgs <- sgl_to_rgs("
      visualize
        bin(mpg) as x,
        count(*) as y,
        bin(mpg) as color
      from cars
      group by
        bin(mpg)
      collect by
        bin(mpg)
      using violins
      scale by
        log(color)
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(rsgl.log.bin.30.mpg, rsgl.linear.bin.30.mpg)"
    )
  }
)

test_that(
  paste(
    "ggplot_aes doesn't include group aes",
    "if no default group cols"
  ),
  {
    rgs <- sgl_to_rgs("
      visualize
        price as y
      from diamonds
      using violin
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_false("group" %in% names(actual_aes))
  }
)

test_that(
  paste(
    "ggplot_aes includes group aes with",
    "expr for default group cols"
  ),
  {
    rgs <- sgl_to_rgs("
      visualize
        cut as x,
        price as y,
        clarity as color
      from diamonds
      using violins
    ")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    layer <- rgs$layers[[1]]

    actual_aes <- ggplot_aes(layer$geom_expr$geom, layer, df, rgs$scales)

    expect_true("group" %in% names(actual_aes))
    expect_equal(
      ggplot2::as_label(actual_aes$group),
      "interaction(cut, clarity)"
    )
  }
)

describe("has_direction", {
  it("returns TRUE", {
    expect_true(
      has_direction(test_violin)
    )
  })
})

describe("ggplot_orntn_from_qual", {
  it("returns x for vertical qual", {
    expect_equal(
      ggplot_orntn_from_qual(test_violin, "vertical"),
      "x"
    )
  })
  it("returns y for horizontal qual", {
    expect_equal(
      ggplot_orntn_from_qual(test_violin, "horizontal"),
      "y"
    )
  })
})
