test_identity <- new_sgl_cta_identity()

describe("new_sgl_cta_identity", {
  it("returns an instance of the sgl_cta_identity subclass", {
    expect_equal(
      attr(test_identity, "class"),
      c("sgl_cta_identity", "sgl_cta")
    )
  })

  it("only adds the class attribute", {
    expect_equal(
      names(attributes(test_identity)),
      "class"
    )
  })

  it("sets the empty list as the base object", {
    expect_equal(
      unclass(test_identity),
      list()
    )
  })
})

describe("valid_cta", {
  it("raises error for *", {
    col_expr <- list(
      column = "*",
      cta = test_identity
    )
    expect_error(
      valid_cta(test_identity, col_expr, data.frame()),
      "Error: '*' can only be used inside an aggregation function",
      fixed = TRUE
    )
  })

  it("doesn't raise error for non-* column", {
    col_expr <- list(
      column = "col",
      cta = test_identity
    )

    expect_no_error(
      valid_cta(test_identity, col_expr, data.frame())
    )
  })
})

describe("is_aggregation", {
  it("returns false", {
    expect_false(is_aggregation(test_identity))
  })
})

describe("is_transformation", {
  it("returns false", {
    expect_false(is_transformation(test_identity))
  })
})
