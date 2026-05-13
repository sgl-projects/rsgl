describe("raise_if_arg_present", {
  describe("arg is not present", {
    it("doesn't raise error", {
      col_expr <- list(
        column = "col_1",
        cta = new_sgl_cta_avg()
      )

      expect_no_error(raise_if_arg_present(col_expr))
    })
  })
  describe("arg is present", {
    it("raises error", {
      col_expr <- list(
        column = "col_1",
        cta = new_sgl_cta_avg(),
        arg = 5
      )

      expect_error(
        raise_if_arg_present(col_expr),
        "Error: avg function received unexpected argument.",
        fixed = TRUE
      )
    })
  })
})
