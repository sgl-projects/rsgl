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
    expect_error(
      valid_cta(test_identity, "*", data.frame()),
      "Error: '*' can only be used inside an aggregation function",
      fixed = TRUE
    )
  })

  it("doesn't raise error for non-* column", {
    expect_no_error(
      valid_cta(test_identity, "col", data.frame())
    )
  })
})

describe("is_transformation", {
  it("returns false", {
    expect_false(is_transformation(test_identity))
  })
})
