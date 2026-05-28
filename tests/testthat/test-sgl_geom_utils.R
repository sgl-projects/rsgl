describe("group_aes_expr", {
  describe("single group col", {
    it("returns col name as symbol", {
      expect_equal(
        group_aes_expr("col_1"),
        as.symbol("col_1")
      )
    })
  })
  describe("multiple group cols", {
    it("returns col names in interaction", {
      group_cols <- c("col_1", "col_2")

      expected_expr <- parse(text = "interaction(col_1, col_2)")[[1]]
      expect_equal(
        group_aes_expr(group_cols),
        expected_expr
      )
    })
  })
})
