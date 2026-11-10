# Build fast GxE mean matrix and additive interaction matrix

Build fast GxE mean matrix and additive interaction matrix

## Usage

``` r
build_ge_matrices(
  df_std,
  gen_col,
  env_col,
  trait,
  rep_col = NULL,
  block_col = NULL
)
```

## Arguments

- df_std:

  Cleaned data frame

- gen_col:

  Genotype column name

- env_col:

  Environment column name

- trait:

  Trait column name

- rep_col:

  Optional replicate column for estimating plot error

- block_col:

  Optional incomplete block column

## Value

List with ge_means, interaction, means, dims and labels
