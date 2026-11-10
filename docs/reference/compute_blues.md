# Compute design-adjusted genotype BLUEs with G x E

Compute design-adjusted genotype BLUEs with G x E

## Usage

``` r
compute_blues(
  df,
  gen_col,
  env_col,
  rep_col,
  trait,
  block_col = NULL,
  design_type = "RCBD",
  augmented_checks = NULL,
  direction = "higher"
)
```

## Arguments

- df:

  Data frame

- gen_col, env_col, rep_col:

  Genotype, environment and replicate columns

- trait:

  Numeric trait column

- block_col:

  Optional incomplete block column

- design_type:

  Experimental design label

- augmented_checks:

  Replicated checks for augmented designs

- direction:

  Selection direction: higher or lower

## Value

Genotype estimates and confidence intervals
