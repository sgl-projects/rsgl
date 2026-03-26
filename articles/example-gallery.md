# Example Gallery

A collection of SGL examples organized by plot type. Each example is a
self-contained SGL statement you can copy and adapt. For a full syntax
reference, see
[`vignette("sgl-language-guide")`](https://sgl-projects.github.io/rsgl/articles/sgl-language-guide.md).

## Scatterplots

### Basic scatterplot

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points
")
```

![](example-gallery_files/figure-html/scatter-basic-1.png)

### Colored by group

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y,
    cyl as color
  from cars
  using points
")
```

![](example-gallery_files/figure-html/scatter-color-1.png)

### Sized by variable

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y,
    wt as size
  from cars
  using points
")
```

![](example-gallery_files/figure-html/scatter-size-1.png)

### With regression line

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using (
    points
    layer
    regression line
  )
")
```

![](example-gallery_files/figure-html/scatter-regression-1.png)

### Jittered points

``` r
set.seed(42)
dbGetPlot(con, "
  visualize
    cyl_cat as x,
    mpg as y
  from (
    select mpg, cast(cyl as varchar) as cyl_cat
    from cars
  )
  using jittered points
")
```

![](example-gallery_files/figure-html/scatter-jittered-1.png)

### Filtered with SQL subquery

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y,
    cyl as color
  from (
    select
      hp, mpg,
      cyl::varchar as cyl
    from cars
    where cyl = 4 or cyl = 6
  )
  using points
")
```

![](example-gallery_files/figure-html/scatter-subquery-1.png)

## Bar charts

### Counts per category

``` r
dbGetPlot(con, "
  visualize
    cut as x,
    count(*) as y
  from diamonds
  group by
    cut
  using bars
")
```

![](example-gallery_files/figure-html/bar-count-1.png)

### Stacked bars

``` r
dbGetPlot(con, "
  visualize
    cut as x,
    count(*) as y,
    color as color
  from diamonds
  group by
    cut, color
  using bars
")
```

![](example-gallery_files/figure-html/bar-stacked-1.png)

### Unstacked (dodged) bars

``` r
dbGetPlot(con, "
  visualize
    cut as x,
    count(*) as y,
    color as color
  from diamonds
  group by
    cut, color
  using unstacked bars
")
```

![](example-gallery_files/figure-html/bar-unstacked-1.png)

## Histograms

### Basic histogram

``` r
dbGetPlot(con, "
  visualize
    bin(mpg) as x,
    count(*) as y
  from cars
  group by
    bin(mpg)
  using bars
")
```

![](example-gallery_files/figure-html/hist-basic-1.png)

### Stacked histogram by group

``` r
dbGetPlot(con, "
  visualize
    bin(mpg) as x,
    count(*) as y,
    cyl_cat as color
  from (
    select *, cast(cyl as varchar) as cyl_cat
    from cars
  )
  group by
    bin(mpg), cyl_cat
  using bars
")
```

![](example-gallery_files/figure-html/hist-grouped-1.png)

## Line plots

### Single line

``` r
dbGetPlot(con, "
  visualize
    age as x,
    circumference as y
  from trees
  using line
")
```

![](example-gallery_files/figure-html/line-single-1.png)

### Multiple lines with collect by

``` r
dbGetPlot(con, "
  visualize
    age as x,
    circumference as y
  from trees
  collect by
    Tree
  using lines
")
```

![](example-gallery_files/figure-html/line-multiple-1.png)

### Regression lines by group

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y,
    cyl as color
  from (
    select hp, mpg, cyl::varchar as cyl
    from cars
  )
  using regression lines
")
```

![](example-gallery_files/figure-html/line-regression-grouped-1.png)

## Box plots

### Basic box plot

``` r
dbGetPlot(con, "
  visualize
    cut as x,
    price as y
  from diamonds
  using boxes
")
```

![](example-gallery_files/figure-html/box-basic-1.png)

## Pie charts

A pie chart is a stacked bar chart in polar coordinates. Map a count to
`theta` and a category to `color`:

``` r
dbGetPlot(con, "
  visualize
    count(*) as theta,
    cut as color
  from diamonds
  group by
    cut
  using bars
")
```

![](example-gallery_files/figure-html/pie-1.png)

## Faceted plots

### Horizontal facets

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points
  facet by
    cyl
")
```

![](example-gallery_files/figure-html/facet-horizontal-1.png)

### Vertical facets

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points
  facet by
    cyl vertically
")
```

![](example-gallery_files/figure-html/facet-vertical-1.png)

### Facet grid

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points
  facet by
    cyl,
    am
")
```

![](example-gallery_files/figure-html/facet-grid-1.png)

## Log-scaled plots

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using (
    points
    layer
    regression line
  )
  scale by
    log(x),
    log(y)
")
```

![](example-gallery_files/figure-html/log-scale-1.png)

## Multi-layer plots

### Points and regression line

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y
  from cars
  using points

  layer

  visualize
    hp as x,
    mpg as y
  from cars
  using regression line
")
```

![](example-gallery_files/figure-html/multi-layer-basic-1.png)

### Custom titles

``` r
dbGetPlot(con, "
  visualize
    hp as x,
    mpg as y,
    cyl as color
  from cars
  using points
  title
    x as 'Horsepower',
    y as 'Miles Per Gallon',
    color as 'Cylinders'
")
```

![](example-gallery_files/figure-html/multi-with-titles-1.png)
