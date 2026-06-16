describe("col_has_match_in_df", {
  df <- data.frame(
    CoL_1 = character(0),
    col_2 = character(0)
  )
  it("returns true for case-insensitive match", {
    col_expr <- list(
      column = "col_1",
      cta = new_sgl_cta_identity()
    )
    expect_true(col_has_match_in_df(col_expr, df))
  })
  it("returns false for no case-insensitive match", {
    col_expr <- list(
      column = "col_3",
      cta = new_sgl_cta_identity()
    )
    expect_false(col_has_match_in_df(col_expr, df))
  })
})

describe("facet_has_match_in_df", {
  df <- data.frame(
    CoL_1 = character(0),
    col_2 = character(0)
  )
  it("returns true for case-insensitive match", {
    facet_expr <- list(
      column = "col_1",
      direction = "default"
    )
    expect_true(facet_has_match_in_df(facet_expr, df))
  })
  it("returns false for no case-insensitive match", {
    facet_expr <- list(
      column = "col_3",
      direction = "default"
    )
    expect_false(facet_has_match_in_df(facet_expr, df))
  })
})

describe("match_col_to_df", {
  df <- data.frame(
    CoL_1 = character(0),
    col_2 = character(0)
  )
  it("matches df column if case-insensitive match found", {
    col_expr <- list(
      column = "col_1",
      cta = new_sgl_cta_identity()
    )

    actual <- match_col_to_df(col_expr, df)

    expected <- list(
      column = "CoL_1",
      cta = new_sgl_cta_identity()
    )
    expect_equal(
      actual,
      expected
    )
  })
  it("matches first column for multiple matches", {
    col_expr <- list(
      column = "col_1",
      cta = new_sgl_cta_identity()
    )
    test_df <- df
    test_df$col_1 <- character(0)
    actual <- match_col_to_df(col_expr, df)

    expected <- list(
      column = "CoL_1",
      cta = new_sgl_cta_identity()
    )
    expect_equal(
      actual,
      expected
    )
  })
  it("returns original column if no case-insensitive match found", {
    col_expr <- list(
      column = "col_3",
      cta = new_sgl_cta_identity()
    )

    actual <- match_col_to_df(col_expr, df)

    expected <- col_expr
    expect_equal(
      actual,
      expected
    )
  })
})

describe("match_facet_to_df", {
  it("matches first df column match found", {
    df <- data.frame(
      CoL_1 = character(0),
      col_2 = character(0),
      col_1 = character(0)
    )
    facet_expr <- list(
      column = "col_1",
      direction = "default"
    )

    actual <- match_facet_to_df(facet_expr, df)

    expected <- list(
      column = "CoL_1",
      direction = "default"
    )
    expect_equal(
      actual,
      expected
    )
  })
})

describe("match_col_casing_for_facet", {
  df_1 <- data.frame(
    col_1 = character(0)
  )
  df_2 <- data.frame(
    col_1 = character(0),
    CoL_2 = character(0)
  )
  df_3 <- data.frame(
    col_2 = character(0),
    col_3 = character(0)
  )
  dfs <- list(df_1, df_2, df_3)
  describe("column has case-insensitive match", {
    it("matches col to first df with a match", {
      facet_expr <- list(
        column = "col_2",
        direction = "default"
      )

      actual <- match_col_casing_for_facet(facet_expr, dfs)

      expected <- list(
        column = "CoL_2",
        direction = "default"
      )
      expect_equal(
        actual,
        expected
      )
    })
  })
  describe("column has no case-insensitive match", {
    it("returns column as is", {
      facet_expr <- list(
        column = "not_a_match",
        direction = "default"
      )

      actual <- match_col_casing_for_facet(facet_expr, dfs)

      expected <- facet_expr
      expect_equal(
        actual,
        expected
      )
    })
  })
})

describe("match_cols_to_df", {
  df <- data.frame(
    CoL_1 = character(0),
    CoL_2 = character(0),
    col_3 = character(0),
    col_1 = character(0)
  )
  col_exprs <- list(
    list(
      column = "col_1",
      cta = new_sgl_cta_identity()
    ),
    list(
      column = "col_2",
      cta = new_sgl_cta_identity()
    ),
    list(
      column = "not_a_match",
      cta = new_sgl_cta_identity()
    )
  )
  matched_col_exprs <- list(
    list(
      column = "CoL_1",
      cta = new_sgl_cta_identity()
    ),
    list(
      column = "CoL_2",
      cta = new_sgl_cta_identity()
    ),
    list(
      column = "not_a_match",
      cta = new_sgl_cta_identity()
    )
  )
  describe("col_exprs are unnamed", {
    it("matches df casing where matches found", {
      actual <- match_cols_to_df(col_exprs, df)

      expect_equal(actual, matched_col_exprs)
    })
  })
  describe("col_exprs are named", {
    it("matches df casing where matches found and preserves names", {
      named_col_exprs <- col_exprs
      col_expr_names <- c("x", "y", "color")
      names(named_col_exprs) <- col_expr_names

      actual <- match_cols_to_df(named_col_exprs, df)

      expected <- matched_col_exprs
      names(expected) <- col_expr_names
      expect_equal(
        actual,
        expected
      )
    })
  })
})

describe("match_col_casing_for_layer", {
  df <- data.frame(
    CoL_1 = character(0),
    CoL_2 = character(0),
    col_3 = character(0),
    col_1 = character(0)
  )
  it("matches column casing for each clause", {
    rgs <- sgl_to_rgs("
			visualize
				col_1 as x,
				col_2 as y,
				not_a_match as color
			from placeholder
			group by
				col_1,
				col_2,
				not_a_match
			collect by
				col_1,
				col_2,
				not_a_match
			using points
		")
    layer <- rgs$layers[[1]]

    actual <- match_col_casing_for_layer(layer, df)

    expected <- sgl_to_rgs("
			visualize
				CoL_1 as x,
				CoL_2 as y,
				not_a_match as color
			from placeholder
			group by
				CoL_1,
				CoL_2,
				not_a_match
			collect by
				CoL_1,
				CoL_2,
				not_a_match
			using points
		")$layers[[1]]

    expect_equal(actual, expected)
  })
  it("ignores grouping and collection clauses if ommitted", {
    rgs <- sgl_to_rgs("
			visualize
				col_1 as x,
				col_2 as y,
				not_a_match as color
			from placeholder
			using points
		")
    layer <- rgs$layers[[1]]

    actual <- match_col_casing_for_layer(layer, df)

    expected <- sgl_to_rgs("
			visualize
				CoL_1 as x,
				CoL_2 as y,
				not_a_match as color
			from placeholder
			using points
		")$layers[[1]]

    expect_equal(actual, expected)
  })
})

describe("match_col_casing_for_layers", {
  df_1 <- data.frame(
    CoL_1 = character(0),
    col_2 = character(0)
  )
  df_2 <- data.frame(
    CoL_3 = character(0),
    col_4 = character(0)
  )
  dfs <- list(df_1, df_2)
  it("matches column casing for each layer", {
    rgs <- sgl_to_rgs("
			visualize
				col_1 as x,
				not_a_match as y
			from placeholder
			group by
				col_1,
				not_a_match
			collect by
				col_1,
				not_a_match
			using points

			layer

			visualize
				col_3 as x,
				not_a_match as y
			from placeholder
			group by
				col_3,
				not_a_match
			collect by
				col_3,
				not_a_match
			using points
		")
    layers <- rgs$layers

    actual <- match_col_casing_for_layers(layers, dfs)

    expected <- sgl_to_rgs("
			visualize
				CoL_1 as x,
				not_a_match as y
			from placeholder
			group by
				CoL_1,
				not_a_match
			collect by
				CoL_1,
				not_a_match
			using points

			layer

			visualize
				CoL_3 as x,
				not_a_match as y
			from placeholder
			group by
				CoL_3,
				not_a_match
			collect by
				CoL_3,
				not_a_match
			using points
		")$layers

    expect_equal(actual, expected)
  })
})

describe("match_col_casing_for_facets", {
  df_1 <- data.frame(
    CoL_1 = character(0),
    col_2 = character(0)
  )
  df_2 <- data.frame(
    CoL_3 = character(0),
    col_4 = character(0)
  )
  dfs <- list(df_1, df_2)
  it("matches column casing for each facet", {
    rgs <- sgl_to_rgs("
			visualize
				placeholder as x
			from placeholder
			using points
			facet by
				col_3 horizontally,
				not_a_match
		")
    facets <- rgs$facets

    actual <- match_col_casing_for_facets(facets, dfs)

    expected <- sgl_to_rgs("
			visualize
				placeholder as x
			from placeholder
			using points
			facet by
				CoL_3 horizontally,
				not_a_match
		")$facets

    expect_equal(actual, expected)
  })
})

describe("match_col_casing", {
  df_1 <- data.frame(
    CoL_1 = character(0),
    col_2 = character(0)
  )
  df_2 <- data.frame(
    CoL_3 = character(0),
    col_4 = character(0)
  )
  dfs <- list(df_1, df_2)
  it("matches column casing for layers and facets", {
    rgs <- sgl_to_rgs("
			visualize
				col_1 as x,
				not_a_match as y
			from placeholder
			group by
				col_1,
				not_a_match
			collect by
				col_1,
				not_a_match
			using points

			layer

			visualize
				col_3 as x,
				not_a_match as y
			from placeholder
			group by
				col_3,
				not_a_match
			collect by
				col_3,
				not_a_match
			using points

			facet by
				col_1 horizontally,
				not_a_match
		")

    actual <- match_col_casing(rgs, dfs)

    expected <- sgl_to_rgs("
			visualize
				CoL_1 as x,
				not_a_match as y
			from placeholder
			group by
				CoL_1,
				not_a_match
			collect by
				CoL_1,
				not_a_match
			using points

			layer

			visualize
				CoL_3 as x,
				not_a_match as y
			from placeholder
			group by
				CoL_3,
				not_a_match
			collect by
				CoL_3,
				not_a_match
			using points

			facet by
				CoL_1 horizontally,
				not_a_match
		")

    expect_equal(actual, expected)
  })
  it("ignores facets if no facet by clause", {
    rgs <- sgl_to_rgs("
			visualize
				col_1 as x,
				not_a_match as y
			from placeholder
			using points
		")
    test_dfs <- list(df_1)

    actual <- match_col_casing(rgs, test_dfs)

    expected <- sgl_to_rgs("
			visualize
				CoL_1 as x,
				not_a_match as y
			from placeholder
			using points
		")

    expect_equal(actual, expected)
  })
})
