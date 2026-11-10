# Compute equal-environment genotype BLUPs and prediction intervals

Compute equal-environment genotype BLUPs and prediction intervals

## Usage

``` r
compute_blups(
  df,
  gen_col,
  env_col,
  rep_col,
  trait,
  block_col = NULL,
  design_type = "RCBD",
  augmented_checks = NULL,
  direction = "higher",
  mixed_result = NULL
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

- mixed_result:

  Optional existing result from run_mixed_model

## Value

Genotype predictions and approximate prediction intervals
