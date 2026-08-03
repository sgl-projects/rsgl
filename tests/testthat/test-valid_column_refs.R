describe("all_column_refs", {
  describe("only visualize clause", {
    it("returns all refs from visualize clause", {
      rgs <- sgl_to_rgs("
        visualize
          mpg as x,
          cyl as y
        from cars
        using points
      ")
      layer <- rgs$layers[[1]]

      expect_equal(
        sort(all_column_refs(layer)),
        sort(c("mpg", "cyl"))
      )
    })
  })
  describe("only visualize and group by clauses", {
    it("returns all refs from visualize and group by clauses", {
      rgs <- sgl_to_rgs("
        visualize
          mpg as x,
          cyl as y
        from cars
        group by
          disp,
          hp
        using points
      ")
      layer <- rgs$layers[[1]]

      expect_equal(
        sort(all_column_refs(layer)),
        sort(c("mpg", "cyl", "disp", "hp"))
      )
    })
  })
  describe("only visualize and collect by clauses", {
    it("returns all refs from visualize and collect by clauses", {
      rgs <- sgl_to_rgs("
        visualize
          mpg as x,
          cyl as y
        from cars
        collect by
          disp,
          hp
        using points
      ")
      layer <- rgs$layers[[1]]

      expect_equal(
        sort(all_column_refs(layer)),
        sort(c("mpg", "cyl", "disp", "hp"))
      )
    })
  })
  describe("all three clauses", {
    it("returns all refs from all three clauses", {
      rgs <- sgl_to_rgs("
        visualize
          mpg as x,
          cyl as y
        from cars
        group by
          disp,
          hp
        collect by
          drat,
          wt
        using points
      ")
      layer <- rgs$layers[[1]]

      expect_equal(
        sort(all_column_refs(layer)),
        sort(c("mpg", "cyl", "disp", "hp", "drat", "wt"))
      )
    })
  })
  it("doesn't return duplicates", {
    rgs <- sgl_to_rgs("
      visualize
        mpg as x,
        cyl as y
      from cars
      group by
        mpg,
        cyl
      collect by
        mpg,
        cyl
      using points
    ")
    layer <- rgs$layers[[1]]

    expect_equal(
      sort(all_column_refs(layer)),
      sort(c("mpg", "cyl"))
    )
  })
})

describe("column_exists", {
  test_df <- DBI::dbGetQuery(test_con, "select * from cars")
  describe("single ref", {
    describe("ref is the wildcard", {
      it("returns TRUE", {
        expect_equal(
          column_exists("*", test_df),
          c("*" = TRUE)
        )
      })
    })
    describe("ref is not the wildcard", {
      describe("column does exist", {
        it("returns TRUE", {
          expect_equal(
            column_exists("mpg", test_df),
            c(mpg = TRUE)
          )
        })
      })
      describe("column doesn't exist", {
        it("returns FALSE", {
          expect_equal(
            column_exists("not_a_col", test_df),
            c(not_a_col = FALSE)
          )
        })
      })
    })
  })
  describe("multiple refs", {
    it("determines existence correctly for each ref", {
      expect_equal(
        column_exists(c("*", "mpg", "not_a_col"), test_df),
        c("*" = TRUE, mpg = TRUE, not_a_col = FALSE)
      )
    })
  })
})

describe("raise_if_col_missing", {
  describe("no columns are missing", {
    it("doesn't raise error", {
      expect_no_error(
        raise_if_col_missing(c("mpg" = TRUE, "cyl" = TRUE))
      )
    })
  })
  describe("column are missing", {
    it("raises error that specifies first missing column", {
      expect_error(
        raise_if_col_missing(c("mpg" = TRUE, "cyl" = FALSE, "disp" = FALSE)),
        "Error: referenced column 'cyl' not found",
        fixed = TRUE
      )
    })
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
  it("allows missing clauses", {
    rgs <- sgl_to_rgs("
      visualize
        cut as x,
        clarity as y
      from diamonds
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
