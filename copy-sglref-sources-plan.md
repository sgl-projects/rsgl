# Plan: Copy sglref Source Files (scanner, parser, C tests) into rsgl

## Motivation

When rsgl was derived from sglref, `scanner.l`, `parser.y`, the
`src/test/` directory (C unit tests), and the root `makefile` were
deliberately excluded because they aren’t needed for the R package
build. However, since sglref is private and rsgl is open source, people
have no way to see the actual scanner and parser source or the C-level
test suite. Including them in the repo (while excluding them from the R
package tarball) gives full visibility into the implementation.

## Files to Copy

All files below are verbatim copies – none contain any `sglref`
references.

| Source (sglref)              | Destination (rsgl)           | Purpose                                                  |
|------------------------------|------------------------------|----------------------------------------------------------|
| `src/parser.y`               | `src/parser.y`               | Bison grammar (generates `parser.tab.c`)                 |
| `src/scanner.l`              | `src/scanner.l`              | Flex lexer (generates `scanner.c`)                       |
| `src/test/test_sgl_to_cgs.c` | `src/test/test_sgl_to_cgs.c` | Parser integration tests (Criterion)                     |
| `src/test/test_aes.c`        | `src/test/test_aes.c`        | Aesthetic enum tests                                     |
| `src/test/test_geom.c`       | `src/test/test_geom.c`       | Geometry enum tests                                      |
| `src/test/test_cta.c`        | `src/test/test_cta.c`        | CTA enum tests                                           |
| `src/test/test_qual.c`       | `src/test/test_qual.c`       | Qualifier enum tests                                     |
| `src/test/test_scale.c`      | `src/test/test_scale.c`      | Scale enum tests                                         |
| `src/test/test_keyword.c`    | `src/test/test_keyword.c`    | Keyword lookup tests                                     |
| `src/test/stubs.c`           | `src/test/stubs.c`           | `Rf_error` stub for standalone test binary               |
| `makefile`                   | `makefile`                   | Dev makefile for parser/scanner regeneration and C tests |

**Total: 11 files** (2 source, 8 test, 1 build)

## Exclusion from the R Package Build

These files must be in the git repo but excluded from the tarball
produced by `R CMD build`. This is done via `.Rbuildignore`, which uses
Perl-style regexes matched against paths relative to the package root.

**Add to `.Rbuildignore`:**

    ^makefile$
    ^src/parser\.y$
    ^src/scanner\.l$
    ^src/test$

The existing `src/Makevars` only lists the specific `OBJECTS` needed for
the R shared library and does not reference `src/test/`, `parser.y`, or
`scanner.l`, so no changes are needed there.

## Git Ignore for Build Artifacts

The `src/.gitignore` already contains `*.o`, `*.so`, and `*.dll`, which
covers compiled objects in subdirectories (git applies parent
`.gitignore` rules recursively). However, the C test binary
(`src/test/test`) also needs to be ignored.

**Create `src/test/.gitignore`:**

    test

This ignores the compiled test binary produced by `make test`.

## Verification

After copying files and updating `.Rbuildignore`:

1.  **`R CMD build .`** – Verify the tarball does NOT contain
    `parser.y`, `scanner.l`, `src/test/`, or `makefile`. Inspect with:

    ``` bash
    tar tzf rsgl_*.tar.gz | grep -E '(parser\.y|scanner\.l|src/test|makefile)'
    ```

    This should return no results.

2.  **`R CMD check --as-cran rsgl_*.tar.gz`** – Should pass with the
    same result as before (0 errors, 0 warnings, 1 NOTE for new
    submission).

3.  **`make test`** (optional, requires `gcc`, `bison`, `flex`,
    `criterion`) – Verify the C test suite compiles and passes from the
    rsgl directory. This confirms the makefile works in the rsgl
    context.

## Commands (Initial Copy)

``` bash
cd /Users/jochapjo/sgl_projects/rsgl

# Copy scanner and parser source
cp ../sglref/src/parser.y src/parser.y
cp ../sglref/src/scanner.l src/scanner.l

# Copy C test directory
mkdir -p src/test
cp ../sglref/src/test/stubs.c src/test/
cp ../sglref/src/test/test_sgl_to_cgs.c src/test/
cp ../sglref/src/test/test_aes.c src/test/
cp ../sglref/src/test/test_cta.c src/test/
cp ../sglref/src/test/test_geom.c src/test/
cp ../sglref/src/test/test_qual.c src/test/
cp ../sglref/src/test/test_scale.c src/test/
cp ../sglref/src/test/test_keyword.c src/test/

# Copy root makefile
cp ../sglref/makefile makefile

# Create .gitignore for test build artifacts
echo "test" > src/test/.gitignore
```

## Follow-Up: sync-sglref Skill Update (Separate Task)

After the initial copy, the sync-sglref skill
(`.claude/skills/sync-sglref/SKILL.md`) needs to be updated so that
future syncs propagate changes to these files instead of skipping them.
Specifically, in the Step 2 classification table:

- Move `parser.y`, `scanner.l` from **sglref-only** (skip) to
  **Direct-copy**
- Move `src/test/*` from **sglref-only** (skip) to **Direct-copy**
- Move `makefile` from **sglref-only** (skip) to **Direct-copy**

These files have no `sglref` references, so they are true direct copies
with no name adjustments needed.
