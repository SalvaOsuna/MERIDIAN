# Fit a design-aware mixed MET model and generalized heritability

Fit a design-aware mixed MET model and generalized heritability

## Usage

``` r
run_mixed_model(
  df,
  gen_col,
  env_col,
  rep_col,
  trait,
  block_col = NULL,
  design_type = "RCBD",
  augmented_checks = NULL
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

## Value

Model, variance components, prediction covariance and Cullis H2
