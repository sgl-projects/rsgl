cat_bin_cases <- function() {
  tibble::tribble(
    ~.test_name, ~expr, ~expected_result,
    "num", "number", FALSE,
    "cat", "letter", TRUE,
    "tmp", "day", FALSE,
    "binned num", "bin(number)", TRUE,
    "binned tmp", "bin(day)", TRUE,
    "count", "count(*)", FALSE
  )
}
patrick::with_parameters_test_that(
  "determines if mapping is categorical or binned:",
  {
    sgl <- sprintf(
      "
        visualize
          %s as x
        from synth
        using points
      ",
      expr
    )
    rgs <- sgl_to_rgs(sgl)
    layer <- rgs$layers[[1]]
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]

    expect_equal(
      is_cat_or_bin_mapping(layer, df, "x"),
      expected_result
    )
  },
  .cases = cat_bin_cases()
)
