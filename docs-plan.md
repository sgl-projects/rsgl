# rsgl Documentation Plan

## Current state

- **README.Rmd**: Minimal — one paragraph, installation, one basic
  scatterplot example
- **man/dbGetPlot.Rd**: Auto-generated, bare-minimum description of the
  single exported function
- **No vignettes, no pkgdown config, no package-level help page
  ([`?rsgl`](https://sgl-projects.github.io/rsgl/reference/rsgl-package.md))**

## The core documentation challenge

rsgl has exactly one exported R function (`dbGetPlot`). The thing users
actually need to learn is the **SGL language itself** — its clauses,
keywords, and grammar. The SGL paper covers this rigorously but is an
academic paper, not a practical guide for R users. The documentation
needs to bridge that gap.

------------------------------------------------------------------------

## Proposed documentation pieces

### 1. Enhanced README (pkgdown home page)

The README is the first thing users see on GitHub and the pkgdown site.
The current version is too sparse to orient someone. It should be
expanded to include:

- A one-sentence pitch: what SGL is and why it exists (a SQL-like
  language for specifying statistical graphics)
- 2-3 example SGL statements with rendered plot output, showing variety
  (a scatterplot, a histogram, and something with layers or facets) so
  visitors immediately grasp the language’s expressiveness
- Installation instructions (already present)
- A link to the SGL paper
- Pointers to the “Get Started” vignette and the language guide for more

This stays concise — it’s a landing page, not a tutorial.

### 2. “Get Started” vignette

The introductory vignette for new users. pkgdown will use this as the
“Get Started” tab if it’s named `rsgl.Rmd`. Contents:

- **What is SGL** — a brief, non-academic explanation. SGL is a graphics
  language designed to feel like SQL. You write a declarative statement,
  rsgl parses it, queries your DuckDB database, and returns a ggplot2
  plot.
- **Setup** — installing the package, creating a DuckDB connection,
  loading example data (the `cars` and `trees` tables used in the paper
  are good canonical examples)
- **Your first plot** — a walkthrough of a simple scatterplot statement,
  breaking down each clause (`visualize`, `from`, `using`)
- **Adding aesthetics** — mapping a third column to color
- **Changing geoms** — switching from points to bars/lines
- **Where to go next** — links to the language guide and gallery

This should be short enough that a user can follow it in 5-10 minutes.

### 3. “SGL Language Guide” vignette

The comprehensive reference for SGL syntax as implemented in rsgl. This
is the single most important piece of documentation. Organized by
clause, mirroring the structure of the paper but written as practical
documentation with runnable R examples:

- **The `from` clause** — specifying tables and SQL subqueries as data
  sources
- **The `visualize` clause** — mapping columns to aesthetics (`x`, `y`,
  `color`, `size`, `theta`, `r`)
- **The `using` clause** — geom types (`points`/`point`, `bars`/`bar`,
  `lines`/`line`, `boxes`/`box`)
- **Column-level transformations and aggregations** — `bin()` and
  `count()` as column expressions, with the `group by` clause
- **The `collect by` clause** — controlling collection behavior for
  collective geoms (line, box)
- **Geom qualifiers** — `jittered`, `regression`, `unstacked`
- **The `layer` operator** — combining multiple layers, including the
  shorthand layered geom expression syntax
- **The `scale by` clause** — `log(x)`, `log(y)`, and the distinction
  between scaling aesthetics vs. transforming data in SQL
- **Coordinate systems** — Cartesian (`x`/`y`) vs. polar (`theta`/`r`),
  and how pie charts are stacked bar charts in polar coordinates
- **The `facet by` clause** — horizontal/vertical panels, grid faceting
  with two expressions
- **The `title` clause** — overriding default axis/legend titles

Each section should have at least one runnable example with plot output.
This vignette will be long, but that’s appropriate — it’s the reference
document.

### 4. “Example Gallery” vignette

A visual cookbook organized by what users want to create, not by
language feature. Each entry is an SGL statement and its rendered plot,
with minimal explanation. Categories:

- **Scatterplots** — basic, colored by group, with regression line
  overlay, jittered
- **Bar charts** — simple counts, stacked, unstacked, grouped
- **Histograms** — using `bin()` + `count()`
- **Line plots** — single series, multiple series with `collect by`,
  regression lines
- **Box plots** — basic, grouped
- **Pie charts** — using polar coordinates
- **Faceted plots** — horizontal, vertical, grid
- **Multi-layer plots** — points + regression, combining different geoms
- **Log-scaled plots** — using `scale by`
- **SQL subqueries** — filtering and transforming data before
  visualization

This serves as both inspiration and copy-paste reference. Users looking
for “how do I make a histogram?” can find it immediately.

### 5. Enhanced function reference

- **Package-level documentation**
  ([`?rsgl`](https://sgl-projects.github.io/rsgl/reference/rsgl-package.md)):
  Add the `"_PACKAGE"` roxygen block to `R/rsgl-package.R` so that
  [`?rsgl`](https://sgl-projects.github.io/rsgl/reference/rsgl-package.md)
  works. This should briefly describe the package and link to the
  vignettes.
- **[`dbGetPlot()`](https://sgl-projects.github.io/rsgl/reference/dbGetPlot.md)
  improvements**: The current man page is functional but terse. Add:
  - A `@details` section explaining the pipeline at a high level (SGL
    string is parsed, data is queried from DuckDB, a ggplot2 object is
    returned)
  - A `@seealso` linking to the vignettes for SGL syntax
  - 2-3 more `@examples` showing different plot types beyond the basic
    scatterplot

------------------------------------------------------------------------

## What NOT to document

- Internal functions (38 R files of internals) — not exported, not
  user-facing, no roxygen needed
- The C/C++ parser internals — implementation detail
- The grammar of graphics theory — the paper handles this; the docs
  should be practical
- Architecture or developer guides — this is user documentation, not
  contributor documentation

------------------------------------------------------------------------

## Suggested implementation order

| Priority | Piece                       | Rationale                                                          |
|----------|-----------------------------|--------------------------------------------------------------------|
| 1        | SGL Language Guide vignette | The most critical gap — users have no way to learn SGL syntax      |
| 2        | Get Started vignette        | New users need an on-ramp                                          |
| 3        | Enhanced README             | First impression on GitHub/pkgdown home                            |
| 4        | Example Gallery vignette    | High value for discoverability, but less urgent than the reference |
| 5        | Enhanced function reference | Low priority since there’s only one function, but easy to do       |
