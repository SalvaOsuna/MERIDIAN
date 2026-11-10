# Create a genotype × environment heatmap of means

Create a genotype × environment heatmap of means

## Usage

``` r
plot_ge_heatmap(
  df,
  gen_col,
  env_col,
  trait,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  palette = "meridian"
)
```

## Arguments

- df:

  Data frame

- gen_col:

  Genotype column

- env_col:

  Environment column

- trait:

  Trait column

- cluster_rows:

  Logical

- cluster_cols:

  Logical

- palette:

  Color palette name for heatmaply

## Value

heatmaply object (htmlwidget) or plotly object
