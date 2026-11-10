# Create boxplots for trait by grouping variable

Create boxplots for trait by grouping variable

## Usage

``` r
plot_boxplots(df, trait, group_col, color_col = NULL, show_points = FALSE)
```

## Arguments

- df:

  Data frame

- trait:

  Trait column name

- group_col:

  Column to group by (environment or genotype)

- color_col:

  Column to color by (optional, defaults to group_col)

- show_points:

  Logical, show jittered points

## Value

ggplot object
