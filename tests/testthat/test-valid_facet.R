describe("valid_facet", {
  describe("number of facets", {
    it("doesn't raise error for zero facets", {
      rgs <- sgl_to_rgs("
        visualize
          day as x,
          number as y
        from synth
        using points
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_no_error(
        valid_facet(rgs, dfs)
      )
    })
    it("doesn't raise error for one facet", {
      rgs <- sgl_to_rgs("
        visualize
          day as x,
          number as y
        from synth
        using points
        facet by
          boolean
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_no_error(
        valid_facet(rgs, dfs)
      )
    })
    it("doesn't raise error for two facets", {
      rgs <- sgl_to_rgs("
        visualize
          day as x,
          number as y
        from synth
        using points
        facet by
          boolean,
          letter
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_no_error(
        valid_facet(rgs, dfs)
      )
    })
    it("raises error for more than two facets", {
      rgs <- sgl_to_rgs("
        visualize
          day as x,
          number as y
        from (
          select
            *,
            not boolean as boolean_opposite
          from synth
        )
        using points
        facet by
          letter,
          boolean,
          boolean_opposite
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_error(
        valid_facet(rgs, dfs),
        "Error: cannot have more than two facets.",
        fixed = TRUE
      )
    })
  })
  describe("facet direction", {
    describe("one facet", {
      it("doesn't raise error for default direction", {
        rgs <- sgl_to_rgs("
          visualize
            day as x,
            number as y
          from synth
          using points
          facet by
            boolean
        ")
        dfs <- result_dfs(rgs, test_con)

        expect_no_error(
          valid_facet(rgs, dfs)
        )
      })
      it("doesn't raise error for horizontal direction", {
        rgs <- sgl_to_rgs("
          visualize
            day as x,
            number as y
          from synth
          using points
          facet by
            boolean horizontally
        ")
        dfs <- result_dfs(rgs, test_con)

        expect_no_error(
          valid_facet(rgs, dfs)
        )
      })
      it("doesn't raise error for vertical direction", {
        rgs <- sgl_to_rgs("
          visualize
            day as x,
            number as y
          from synth
          using points
          facet by
            boolean vertically
        ")
        dfs <- result_dfs(rgs, test_con)

        expect_no_error(
          valid_facet(rgs, dfs)
        )
      })
    })
    describe("two facets", {
      describe("both default directions", {
        it("doesn't raise error", {
          rgs <- sgl_to_rgs("
            visualize
              day as x,
              number as y
            from synth
            using points
            facet by
              boolean,
              letter
          ")
          dfs <- result_dfs(rgs, test_con)

          expect_no_error(
            valid_facet(rgs, dfs)
          )
        })
      })
      describe("one default direction", {
        it("doesn't raise error for horizontal non-default direction", {
          rgs <- sgl_to_rgs("
            visualize
              day as x,
              number as y
            from synth
            using points
            facet by
              boolean,
              letter horizontally
          ")
          dfs <- result_dfs(rgs, test_con)

          expect_no_error(
            valid_facet(rgs, dfs)
          )
        })
        it("doesn't raise error for vertical non-default direction", {
          rgs <- sgl_to_rgs("
            visualize
              day as x,
              number as y
            from synth
            using points
            facet by
              boolean,
              letter vertically
          ")
          dfs <- result_dfs(rgs, test_con)

          expect_no_error(
            valid_facet(rgs, dfs)
          )
        })
      })
      describe("no default directions", {
        it(
          paste(
            "doesn't raise error for one horizontal",
            "and one vertical direction"
          ),
          {
            rgs <- sgl_to_rgs("
              visualize
                day as x,
                number as y
              from synth
              using points
              facet by
                boolean horizontally,
                letter vertically
            ")
            dfs <- result_dfs(rgs, test_con)

            expect_no_error(
              valid_facet(rgs, dfs)
            )
          }
        )
        it("raises error for two horizontal directions", {
          rgs <- sgl_to_rgs("
            visualize
              day as x,
              number as y
            from synth
            using points
            facet by
              letter horizontally,
              boolean horizontally
          ")
          dfs <- result_dfs(rgs, test_con)

          expected_msg <- paste(
            "Error: for two facets, one must be",
            "horizontal and the other vertical."
          )
          expect_error(
            valid_facet(rgs, dfs),
            expected_msg,
            fixed = TRUE
          )
        })
        it("raises error for two vertical directions", {
          rgs <- sgl_to_rgs("
            visualize
              day as x,
              number as y
            from synth
            using points
            facet by
              letter vertically,
              boolean vertically
          ")
          dfs <- result_dfs(rgs, test_con)

          expected_msg <- paste(
            "Error: for two facets, one must be",
            "horizontal and the other vertical."
          )
          expect_error(
            valid_facet(rgs, dfs),
            expected_msg,
            fixed = TRUE
          )
        })
      })
    })
  })
  describe("column existence", {
    it("doesnt raise error if column exists in at least one layer's source", {
      rgs <- sgl_to_rgs("
        visualize
          day as x,
          number as y
        from synth
        using points

        layer

        visualize
          day as x,
          number as y
        from (
          select
            day,
            number
          from synth
        )
        using points

        facet by
          boolean
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_no_error(
        valid_facet(rgs, dfs)
      )
    })
    it("raises error if column doesn't exists in any layer's source", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x,
          mpg as y
        from cars
        using (
          points
          layer
          regression line
        )
        facet by
          not_a_col
      ")
      dfs <- result_dfs(rgs, test_con)

      expected_msg <- paste(
        "Error: facet column 'not_a_col' does",
        "not exist in any layer data sources."
      )
      expect_error(
        valid_facet(rgs, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
    it("raises error for multiple facets with a column that doesn't exist", {
      rgs <- sgl_to_rgs("
        visualize
          hp as x,
          mpg as y
        from cars
        using (
          points
          layer
          regression line
        )
        facet by
          cyl,
          not_a_col
      ")
      dfs <- result_dfs(rgs, test_con)

      expected_msg <- paste(
        "Error: facet column 'not_a_col' does",
        "not exist in any layer data sources."
      )
      expect_error(
        valid_facet(rgs, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
  })
  describe("column type consistency", {
    it("doesn't raise error if facet column type is consistent across layers", {
      rgs <- sgl_to_rgs("
        visualize
          letter as x,
          day as y
        from synth
        using points

        layer

        visualize
          letter as x,
          day as y
        from synth
        using line

        facet by
          number
      ")
      dfs <- result_dfs(rgs, test_con)

      expect_no_error(
        valid_facet(rgs, dfs)
      )
    })
    it("raises error when facet column has inconsistent type across layers", {
      rgs <- sgl_to_rgs("
        visualize
          letter as x,
          day as y
        from (
          select
            *,
            number as facet_col
          from synth
        )
        using points

        layer

        visualize
          letter as x,
          day as y
        from (
          select
            *,
            day_and_time as facet_col
          from synth
        )
        using points

        facet by
          facet_col
      ")
      dfs <- result_dfs(rgs, test_con)

      expected_msg <- paste(
        "Error: facet column 'facet_col' does not have a",
        "consistent type (categorical, numerical, or temporal)",
        "across all layers where it is present."
      )
      expect_error(
        valid_facet(rgs, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
    it("raises error for multiple facets with an inconsistent type facet", {
      rgs <- sgl_to_rgs("
        visualize
          letter as x,
          day as y
        from (
          select
            *,
            number as facet_col
          from synth
        )
        using points

        layer

        visualize
          letter as x,
          day as y
        from (
          select
            *,
            day_and_time as facet_col
          from synth
        )
        using points

        facet by
          boolean,
          facet_col
      ")
      dfs <- result_dfs(rgs, test_con)

      expected_msg <- paste(
        "Error: facet column 'facet_col' does not have a",
        "consistent type (categorical, numerical, or temporal)",
        "across all layers where it is present."
      )
      expect_error(
        valid_facet(rgs, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
  })
  describe("column type classification", {
    it("raises error if facet col has unknown type class", {
      DBI::dbBegin(test_con)
      withr::defer(DBI::dbRollback(test_con))
      DBI::dbExecute(test_con, "alter table synth add column blob_col BLOB")

      rgs <- sgl_to_rgs("
        visualize
          letter as x,
          number as y
        from synth
        using line

        facet by
          blob_col
      ")
      dfs <- result_dfs(rgs, test_con)

      expected_msg <- paste(
        "Error: unknown SGL type classification",
        "(numerical, categorical, or temporal)",
        "for column 'blob_col'."
      )
      expect_error(
        valid_facet(rgs, dfs),
        expected_msg,
        fixed = TRUE
      )
    })
  })
})
