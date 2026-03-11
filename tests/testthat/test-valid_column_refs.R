describe("column_exists", {
  describe("column is the wildcard", {
    it("returns TRUE regardless of cta", {
      rgs <- sgl_to_rgs("
				visualize
					* as x,
					bin(*) as y,
					count(*) as color
				from cars
				using points
			")
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]
      col_exprs <- rgs$layers[[1]]$aes_mappings

      expect_equal(
        column_exists(col_exprs, df),
        c("*" = TRUE)
      )
    })
  })
  describe("reference is not the wildcard", {
    describe("column does exist", {
      it("returns TRUE regardless of cta", {
        rgs <- sgl_to_rgs("
					visualize
						hp as x,
						bin(mpg) as y,
						count(cyl) as color
					from cars
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        col_exprs <- rgs$layers[[1]]$aes_mappings

        expect_equal(
          column_exists(col_exprs, df),
          c(cyl = TRUE, mpg = TRUE, hp = TRUE)
        )
      })
    })
    describe("column doesn't exist", {
      it("returns FALSE regardless of cta", {
        rgs <- sgl_to_rgs("
					visualize
						not_a_col_1 as x,
						bin(not_a_col_2) as y,
						count(not_a_col_3) as color
					from cars
					using points
				")
        dfs <- result_dfs(rgs, test_con)
        df <- dfs[[1]]
        col_exprs <- rgs$layers[[1]]$aes_mappings

        expect_equal(
          column_exists(col_exprs, df),
          c(not_a_col_3 = FALSE, not_a_col_2 = FALSE, not_a_col_1 = FALSE)
        )
      })
    })
  })
  it("handles mixture of existing and non-existing columns", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				not_a_col as y,
				* as color
			from cars
			using points
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    col_exprs <- rgs$layers[[1]]$aes_mappings

    expect_equal(
      column_exists(col_exprs, df),
      c("*" = TRUE, not_a_col = FALSE, hp = TRUE)
    )
  })
  it("doesn't duplicate results", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				bin(hp) as y,
				not_a_col_1 as theta,
				count(not_a_col_1) as r,
				bin(*) as color,
				count(*) as size
			from cars
			using points
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    col_exprs <- rgs$layers[[1]]$aes_mappings

    expect_equal(
      column_exists(col_exprs, df),
      c("*" = TRUE, not_a_col_1 = FALSE, hp = TRUE)
    )
  })
  it("works with unnamed col_exprs", {
    rgs <- sgl_to_rgs("
			visualize
				hp as x,
				not_a_col as y,
				* as color
			from cars
			using points
		")
    dfs <- result_dfs(rgs, test_con)
    df <- dfs[[1]]
    col_exprs <- unname(rgs$layers[[1]]$aes_mappings)

    expect_equal(
      column_exists(col_exprs, df),
      c("*" = TRUE, not_a_col = FALSE, hp = TRUE)
    )
  })
})

describe("valid_column_refs", {
  describe("column refs are valid in all clauses", {
    it("doesn't raise error", {
      rgs <- sgl_to_rgs("
				visualize
					cut as x,
					count(*) as y,
					clarity as color
				from diamonds
				group by
					cut,
					clarity
				collect by
					clarity
				using lines
			")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expect_no_error(
        valid_column_refs(layer, df)
      )
    })
  })
  describe("invalid column ref in visualize clause", {
    it("raises error", {
      rgs <- sgl_to_rgs("
				visualize
					cut as x,
					count(not_a_col) as y,
					clarity as color
				from diamonds
				group by
					cut,
					clarity
				collect by
					clarity
				using lines
			")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expect_error(
        valid_column_refs(layer, df),
        "Error: referenced column 'not_a_col' not found",
        fixed = TRUE
      )
    })
  })
  describe("invalid column ref in group by clause", {
    it("raises error", {
      rgs <- sgl_to_rgs("
				visualize
					cut as x,
					count(*) as y,
					clarity as color
				from diamonds
				group by
					cut,
					not_a_col
				collect by
					clarity
				using lines
			")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expect_error(
        valid_column_refs(layer, df),
        "Error: referenced column 'not_a_col' not found",
        fixed = TRUE
      )
    })
  })
  describe("invalid column ref in collect by clause", {
    it("raises error", {
      rgs <- sgl_to_rgs("
				visualize
					cut as x,
					count(*) as y,
					clarity as color
				from diamonds
				group by
					cut,
					clarity
				collect by
					not_a_col
				using lines
			")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expect_error(
        valid_column_refs(layer, df),
        "Error: referenced column 'not_a_col' not found",
        fixed = TRUE
      )
    })
  })
})
