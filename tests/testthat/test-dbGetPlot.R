test_sgl_stmt <- "
  visualize
    HP as x,
    mpg as y
  from cars
  using points

  layer

  visualize
    carat as x,
    price as y
  from diamonds
  using points
"
test_plot <- dbGetPlot(test_con, test_sgl_stmt)

describe("dbGetPlot", {
  it("returns an instance of the sgl_plot class", {
    expect_equal(attr(test_plot, "class"), "sgl_plot")
  })
  it("only adds class attribute", {
    all_attributes <- names(attributes(test_plot))
    expect_setequal(all_attributes, c("names", "class"))
  })
  it("returns matching case rgs with result dfs as base object", {
    rgs <- sgl_to_rgs(test_sgl_stmt)
    dfs <- result_dfs(rgs, test_con)
    expected <- match_col_casing(rgs, dfs)
    expected$result_dfs <- dfs

    expect_equal(
      unclass(test_plot),
      expected
    )
  })
})
