# Run design-adjusted fixed-effects MET ANOVA

Run design-adjusted fixed-effects MET ANOVA

## Usage

``` r
run_anova_met(
  df,
  gen_col,
  env_col,
  rep_col,
  block_col = NULL,
  trait,
  design_type = "RCBD",
  augmented_checks = NULL
)
```

## Arguments

- df:

  Data frame

- gen_col, env_col, rep_col:

  Genotype, environment and replicate columns

- block_col:

  Optional block column nested within replicate and environment

- trait:

  Numeric trait column

- design_type:

  Experimental design label

- augmented_checks:

  Check genotypes for augmented designs

## Value

Adjusted tests and observation counts. Model is an lm object on the
general path, or a list of equivalent environment-specific lm fits on
the optimized path for more than 1000 observations with estimable cell
means.
