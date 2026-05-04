test_identity <- new_sgl_cta_identity()
test_df <- DBI::dbGetQuery(test_con, "select * from synth")

describe("valid_column_class", {
  describe("column is *", {
    it("doesn't raise error", {
      expect_no_error(
        valid_column_class("*", test_df)
      )
    })
  })
  describe("column is numerical", {
    it("doesn't raise error", {
      expect_no_error(
        valid_column_class("number", test_df)
      )
    })
  })
  describe("column is categorical", {
    it("doesn't raise error", {
      expect_no_error(
        valid_column_class("letter", test_df)
      )
    })
  })
  describe("column is temporal", {
    it("doesn't raise error", {
      expect_no_error(
        valid_column_class("day", test_df)
      )
    })
  })
  describe("column class is unknown", {
    it("raises error", {
      DBI::dbBegin(test_con)
      withr::defer(DBI::dbRollback(test_con))
      DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")
      test_df_w_blob <- DBI::dbGetQuery(test_con, "select * from synth")

      expected_msg <- paste(
        "Error: unknown SGL type classification",
        "(numerical, categorical, or temporal)",
        "for column 'blob_col'."
      )
      expect_error(
        valid_column_class("blob_col", test_df_w_blob),
        expected_msg,
        fixed = TRUE
      )
    })
  })
})

describe("valid_column_classes", {
  describe("all classes are valid in all clauses", {
    it("doesn't raise error", {
      rgs <- sgl_to_rgs("
        visualize
          letter as x,
          count(*) as y
        from synth
        group by
          letter
        collect by
          boolean
        using lines
      ")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expect_no_error(
        valid_column_classes(layer, df)
      )
    })
  })
  describe("class in visualize clause is invalid", {
    it("raises error", {
      DBI::dbBegin(test_con)
      withr::defer(DBI::dbRollback(test_con))
      DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")

      rgs <- sgl_to_rgs("
        visualize
          blob_col as x,
          number as y
        from synth
        using points
      ")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expected_msg <- paste(
        "Error: unknown SGL type classification",
        "(numerical, categorical, or temporal)",
        "for column 'blob_col'."
      )
      expect_error(
        valid_column_classes(layer, df),
        expected_msg,
        fixed = TRUE
      )
    })
  })
  describe("class in group by clause is invalid", {
    it("raises error", {
      DBI::dbBegin(test_con)
      withr::defer(DBI::dbRollback(test_con))
      DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")

      rgs <- sgl_to_rgs("
        visualize
          count(*) as x
        from synth
        group by
          blob_col
        using points
      ")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expected_msg <- paste(
        "Error: unknown SGL type classification",
        "(numerical, categorical, or temporal)",
        "for column 'blob_col'."
      )
      expect_error(
        valid_column_classes(layer, df),
        expected_msg,
        fixed = TRUE
      )
    })
  })
  describe("class in collect by clause is invalid", {
    it("raises error", {
      DBI::dbBegin(test_con)
      withr::defer(DBI::dbRollback(test_con))
      DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")

      rgs <- sgl_to_rgs("
        visualize
          letter as x,
          number as y
        from synth
        collect by
          blob_col
        using lines
      ")
      layer <- rgs$layers[[1]]
      dfs <- result_dfs(rgs, test_con)
      df <- dfs[[1]]

      expected_msg <- paste(
        "Error: unknown SGL type classification",
        "(numerical, categorical, or temporal)",
        "for column 'blob_col'."
      )
      expect_error(
        valid_column_classes(layer, df),
        expected_msg,
        fixed = TRUE
      )
    })
  })
})
